#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

void
usage (void)
{
	fprintf (stderr, "usage: ccombine\n");
	exit (1);
}

enum node_type {
	INTEGER,
	FLOAT,
	STRING,
	SYMBOL,
	LIST,
};

struct node {
	enum node_type type;
	struct node *parent;
	double number;
	char *string;
	int list_used, list_avail;
	struct node **list;
};

struct node *
make_node (enum node_type type, struct node *parent)
{
	struct node *ret = calloc (1, sizeof *ret);
	ret->type = type;
	ret->parent = parent;
	return (ret);
}

struct node *
make_integer (int val, struct node *parent)
{
	struct node *ret = make_node(INTEGER, parent);
	ret->number = val;
	return (ret);
}

struct node *
make_float (double val, struct node *parent)
{
	struct node *ret = make_node(FLOAT, parent);
	ret->number = val;
	return (ret);
}

struct node *
make_string (char *val, struct node *parent)
{
	struct node *ret = make_node(STRING, parent);
	ret->string = strdup (val);
	return (ret);
}

struct node *
make_symbol (char *val, struct node *parent)
{
	struct node *ret = make_node(SYMBOL, parent);
	ret->string = strdup (val);
	return (ret);
}

struct node *
make_list (struct node *parent)
{
	return (make_node(LIST, parent));
}

void
discard_whitespace (FILE *f)
{
	int c;
	
	while ((c = getc (f)) != EOF) {
		if (! isspace (c)) {
			ungetc (c, f);
			break;
		}
	}
}

struct node *read_exp (FILE *f, struct node *parent);

void
append (struct node *np, struct node *val)
{
	if (np->list_used >= np->list_avail) {
		np->list_avail += 10;
		np->list = realloc (np->list, np->list_avail * sizeof *np);
	}
	np->list[np->list_used++] = val;
	val->parent = np;
}

struct node *
read_list(FILE *f, struct node *parent)
{
	struct node *ret;
	struct node *exp;

	ret = make_list (parent);
	while ((exp = read_exp (f, ret)) != NULL)
		append (ret, exp);

	getc (f);

	return (ret);
}

char *
read_bytes (FILE *f, int for_string)
{
	char buf[1000];
	int used = 0;
	int c;

	while ((c = getc (f)) != EOF) {
		if (for_string) {
			if (c == '"')
				break;
		} else {
			if (isspace(c))
				break;
			if (c == '(' || c == ')' || c == '"') {
				ungetc (c, f);
				break;
			}
		}
		if (c == '\\') {
			if ((c = getc (f)) == EOF)
				break;
			switch (c) {
			case 'r': c = '\r'; break;
			case 'n': c = '\n'; break;
			case 't': c = '\t'; break;
			default: break;
			}
		}
		if (used + 2 < sizeof buf)
			buf[used++] = c;
	}
	buf[used] = 0;

	return (strdup (buf));
}

struct node *
read_string(FILE *f, struct node *parent)
{
	return (make_string (read_bytes(f, 1), parent));
}
			
struct node *
read_symbol(FILE *f, struct node *parent)
{
	return (make_symbol (read_bytes (f, 0), parent));
}
			
struct node *
read_number(FILE *f, struct node *parent)
{
	double val;
	if (fscanf (f, "%lg", &val) != 1)
		return (NULL);

	struct node *ret = make_float(val, parent);
	if (ret->number - floor (ret->number) == 0)
		ret->type = INTEGER;
	return (ret);
}

struct node *
read_exp (FILE *f, struct node *parent)
{
	discard_whitespace (f);

	int c = getc (f);

	if (c == EOF) {
		return (NULL);
	} else if (c == ')') {
		return (NULL);
	} else if (c == '(') {
		return (read_list(f, parent));
	} else if (c == '"') {
		return (read_string(f, parent));
	} else if (isdigit(c) || c == '-') {
		ungetc (c, f);
		return (read_number(f, parent));
	} else {
		ungetc (c, f);
		return (read_symbol(f, parent));
	}
}

void
print_exp(FILE *outf, struct node *val)
{
	switch (val->type) {
	case STRING:
		putc ('"', outf);
		for (char *p = val->string; *p; p++) {
			switch (*p) {
			case '\r': fprintf (outf, "\\r"); break;
			case '\n': fprintf (outf, "\\n"); break;
			case '\t': fprintf (outf, "\\t"); break;
			case '\\': fprintf (outf, "\\\\"); break;
			case '"': fprintf (outf, "\\\""); break;
			default: putc (*p, outf); break;
			}
		}
		putc ('"', outf);
		break;
	case SYMBOL:
		fprintf (outf, "%s", val->string);
		break;
	case FLOAT:
		fprintf (outf, "%.8g", val->number);
		break;
	case INTEGER:
		fprintf (outf, "%.0f", val->number);
		break;
	case LIST:
		fprintf (outf, "(");
		for (int i = 0; i < val->list_used; i++) {
			print_exp (outf, val->list[i]);
			putc (' ', outf);
		}
		fprintf (outf, ")\n");
		break;
	}
}

struct node *
read_file (char *filename)
{
	FILE *f;

	if ((f = fopen (filename, "r")) == NULL) {
		fprintf (stderr, "can't open %s\n", filename);
		exit (1);
	}

	struct node *ret = read_exp (f, NULL);
	fclose (f);
	return (ret);
}

struct node *
make_empty (void)
{
	struct node *ret, *elt;

	ret = make_list (NULL);
	
	append (ret, make_symbol ("kicad_sch", NULL));

	elt = make_list (NULL);
	append (elt, make_symbol ("version", NULL));
	append (elt, make_symbol ("20200310", NULL));
	append (ret, elt);

	elt = make_list (NULL);
	append (elt, make_symbol("page", NULL));
	append (elt, make_string ("A4", NULL));
	append (ret, elt);

	return (ret);
}

struct sheet_info {
	struct sheet_info *next;
	char *filename;
	struct node *exp;
};

struct sheet_info *
get_sheet (char *filename)
{
	struct sheet_info *si;
	
	si = calloc (1, sizeof *si);
	si->filename = strdup (filename);
	si->exp = read_file (filename);
	print_exp (stdout, si->exp);
	return (si);
}

void
add_sheet (struct node *top, struct node *sheet, char *name)
{
}

int
main (int argc, char **argv)
{
	int c;
	struct node *top;
	FILE *outf;

	while ((c = getopt (argc, argv, "")) != EOF) {
		switch (c) {
		default:
			usage ();
		}
	}

	top = make_empty ();

	remove ("top.sch");
	outf = fopen ("top.sch", "w");
	print_exp (outf, top);
	fclose (outf);

	
	return (0);
}

	
