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

enum obj_type {
	INTEGER,
	FLOAT,
	STRING,
	SYMBOL,
	CONS,
};

struct obj {
	enum obj_type type;
	double number;
	char *string;
	struct obj *car;
	struct obj *cdr;
};

struct obj *
make_obj (enum obj_type type)
{
	struct obj *ret = calloc (1, sizeof *ret);
	ret->type = type;
	return (ret);
}

struct obj *
cons (struct obj *car, struct obj *cdr)
{
	struct obj *ret = make_obj (CONS);
	ret->car = car;
	ret->cdr = cdr;
	return (ret);
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

struct obj *read_exp (FILE *f);

struct obj *
read_list(FILE *f)
{
	struct obj *ret = NULL;
	struct obj **tailp = &ret;
	struct obj *next;
	struct obj *last;

	while ((next = read_exp (f)) != NULL) {
		last = cons (next, NULL);
		*tailp = last;
		tailp = &last->cdr;
	}

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

struct obj *
read_string(FILE *f)
{
	struct obj *ret = make_obj(STRING);
	ret->string = read_bytes(f, 1);
	return (ret);
}
			
struct obj *
read_symbol(FILE *f)
{
	struct obj *ret = make_obj(SYMBOL);
	ret->string = read_bytes(f, 0);
	return (ret);
}
			
struct obj *
read_number(FILE *f)
{
	double val;
	if (fscanf (f, "%lg", &val) != 1)
		return (NULL);

	struct obj *ret = make_obj(FLOAT);
	ret->number = val;
	if (val - floor (val) == 0)
		ret->type = INTEGER;
	return (ret);
}

struct obj *
read_exp (FILE *f)
{
	discard_whitespace (f);

	int c = getc (f);

	if (c == EOF) {
		return (NULL);
	} else if (c == ')') {
		return (NULL);
	} else if (c == '(') {
		return (read_list(f));
	} else if (c == '"') {
		return (read_string(f));
	} else if (isdigit(c) || c == '-') {
		ungetc (c, f);
		return (read_number(f));
	} else {
		ungetc (c, f);
		return (read_symbol(f));
	}
}

void
print_exp(FILE *outf, struct obj *val)
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
	case CONS:
		fprintf (outf, "(");
		for (struct obj *op = val; op; op = op->cdr) {
			print_exp (outf, op->car);
			putc (' ', outf);
		}
		fprintf (outf, ")\n");
		break;
	}
}

struct obj *
read_file (char *filename)
{
	FILE *f;

	if ((f = fopen (filename, "r")) == NULL) {
		fprintf (stderr, "can't open %s\n", filename);
		exit (1);
	}

	struct obj *ret = read_exp (f);
	fclose (f);
	return (ret);
}


int
main (int argc, char **argv)
{
	int c;
	struct obj *empty;

	while ((c = getopt (argc, argv, "")) != EOF) {
		switch (c) {
		default:
			usage ();
		}
	}

	empty = read_file ("empty/empty.kicad_sch");
	print_exp (stdout, empty);

	return (0);
}

	
