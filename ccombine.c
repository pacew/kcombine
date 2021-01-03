#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>

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
make_node (enum node_type type)
{
	struct node *ret = calloc (1, sizeof *ret);
	ret->type = type;
	return (ret);
}

struct node *
make_integer (int val)
{
	struct node *ret = make_node(INTEGER);
	ret->number = val;
	return (ret);
}

struct node *
make_float (double val)
{
	struct node *ret = make_node(FLOAT);
	ret->number = val;
	return (ret);
}

struct node *
make_string (char *val)
{
	struct node *ret = make_node(STRING);
	ret->string = strdup (val);
	return (ret);
}

struct node *
make_symbol (char *val)
{
	struct node *ret = make_node(SYMBOL);
	ret->string = strdup (val);
	return (ret);
}

struct node *
make_list (void)
{
	return (make_node(LIST));
}

struct node *
make_uuid (void)
{
	char buf[1000];
	char *p;
	int i;
	int val;

	p = buf;
	for (i = 0; i < 16; i++) {
		val = random () & 0xff;
		if (i == 6)
			val = 0x40 | (val & 0x0f);
		if (i == 4 || i == 6 || i == 8 || i == 10)
			*p++ = '-';
		p += sprintf (p, "%02x", val);
	}
	*p = 0;
	return (make_symbol(buf));
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
	if (val)
		val->parent = np;
}

struct node *
read_list(FILE *f, struct node *parent)
{
	struct node *ret;
	struct node *exp;

	ret = make_list ();
	ret->parent = parent;
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
read_string(FILE *f)
{
	return (make_string (read_bytes(f, 1)));
}
			
struct node *
read_symbol(FILE *f)
{
	return (make_symbol (read_bytes (f, 0)));
}
			
struct node *
read_number(FILE *f)
{
	double val;
	if (fscanf (f, "%lg", &val) != 1)
		return (NULL);

	struct node *ret = make_float(val);
	if (ret->number - floor (ret->number) == 0)
		ret->type = INTEGER;
	return (ret);
}

struct node *
read_exp (FILE *f, struct node *parent)
{
	struct node *val;
	discard_whitespace (f);

	int c = getc (f);

	if (c == EOF) {
		return (NULL);
	} else if (c == ')') {
		return (NULL);
	} else if (c == '(') {
		return (read_list(f, parent));
	} else if (c == '"') {
		val = read_string(f);
		val->parent = parent;
		return (val);
	} else if (isdigit(c) || c == '-') {
		ungetc (c, f);
		val = read_number(f);
		val->parent = parent;
		return (val);
	} else {
		ungetc (c, f);
		val = read_symbol(f);
		val->parent = parent;
		return (val);
	}
}

void
do_indent (FILE *outf, int n)
{
	int i;
	for (i = 0; i < n; i++)
		putc (' ', outf);
}

void
print_exp(FILE *outf, struct node *val, int indent)
{
	do_indent (outf, indent);
	
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
		putc ('\n', outf);
		break;
	case SYMBOL:
		fprintf (outf, "%s\n", val->string);
		break;
	case FLOAT:
		fprintf (outf, "%.8g\n", val->number);
		break;
	case INTEGER:
		fprintf (outf, "%.0f\n", val->number);
		break;
	case LIST:
		fprintf (outf, "(\n");
		for (int i = 0; i < val->list_used; i++) {
			print_exp (outf, val->list[i], indent + 4);
		}
		do_indent (outf, indent);
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

	ret = make_list ();
	
	append (ret, make_symbol ("kicad_sch"));

	elt = make_list ();
	append (elt, make_symbol ("version"));
	append (elt, make_symbol ("20200310"));
	append (ret, elt);

	elt = make_list ();
	append (elt, make_symbol("page"));
	append (elt, make_string ("A4"));
	append (ret, elt);

	return (ret);
}

struct sheet_info {
	struct sheet_info *next;
	char *filename;
	struct node *exp;
};

struct sheet_info *all_sheets;

struct sheet_info *
get_sheet (char *filename)
{
	struct sheet_info *si;
	
	si = calloc (1, sizeof *si);
	si->filename = strdup (filename);
	si->exp = read_file (filename);

	si->next = all_sheets;
	all_sheets = si;
	
	return (si);
}

struct node *
assoc (struct node *alist, char *key)
{
	int i;
	struct node *elt, *op;
	
	if (alist->type != LIST)
		return (NULL);

	for (i = 0; i < alist->list_used; i++) {
		elt = alist->list[i];
		if (elt->type != LIST)
			continue;
		if (elt->list_used < 2)
			continue;
		op = elt->list[0];
		if (op->type != SYMBOL)
			continue;
		if (strcmp (op->string, key) == 0)
			return (elt);
	}

	return (NULL);
}

struct node *
alist_val (struct node *alist, char *key)
{
	struct node *elt;
	
	if ((elt = assoc (alist, key)) == NULL)
		return (NULL);

	if (elt->type != LIST)
		return (NULL);

	if (elt->list_used < 2)
		return (NULL);

	return (elt->list[1]);
}
		

struct node *
find_sheet (struct node *top, struct sheet_info *sheet, char *inst_name)
{
	int i;
	struct node *elt, *op;
	
	printf ("%d\n", top->list_used);

	for (i = 0; i < top->list_used; i++) {
		elt = top->list[i];
		if (elt->type != LIST)
			continue;
		if (elt->list_used == 0)
			continue;
		op = elt->list[0];
		if (op->type != SYMBOL)
			continue;
		if (strcmp (op->string, "sheet") != 0)
			continue;
		print_exp (stdout, elt, 0);
	}

	return (NULL);
}

/*
(sheet 
  (at 147.32 31.75) 
  (size 13.97 21.59)
  (stroke (width 0.001) (type solid) (color 132 0 132 1))
  (fill (color 255 255 255 0.0000))
  (uuid c293553d-1857-49ba-80c6-4344e9369d2a)
  (property "Sheet name" "led1" (id 0) (at 147.32 31.1141 0)
    (effects (font (size 1.27 1.27)) (justify left bottom)))
  (property "Sheet file" "led.kicad_sch" (id 1) (at 147.32 53.8489 0)
    (effects (font (size 1.27 1.27)) (justify left top)))
)
*/

struct node *
next_sheet_inst (struct node *node, char *name, char *val)
{
	int max_id, i;
	struct node *elt, *op, *id, *at, *elt_name, *match;
	
	if (node->type != LIST) {
		fprintf (stderr, "not a list");
		exit (1);
	}

	for (i = 0; i < node->list_used; i++) {
		elt = node->list[i];

		if (elt->type != LIST)
			continue;
		if (elt->list_used < 3)
			continue;
		op = elt->list[0];
		if (op->type != SYMBOL)
			continue;
		if (strcmp (op->string, "property") != 0)
			continue;

		if ((id = alist_val (elt, "id")) != NULL
		    && id->type == INTEGER) {
			if (id->number > max_id)
				max_id = id->number;
		}
			
		elt_name = elt->list[1];

		if (elt_name->type != STRING)
			continue;
		if (strcmp (elt_name->string, name) != 0)
			continue;

		match = elt;
		break;
	}

	if (match == NULL) {
		match = make_list();
		append (match, make_symbol ("property"));
		append (match, make_string (name));
		append (match, NULL);

		id = make_list ();
		append (id, make_symbol ("id"));
		append (id, make_integer (max_id + 1));
		append (match, id);

		at = make_list ();
		append (at, make_symbol ("at"));
		append (at, make_float (50));
		append (at, make_float (60));
		append (match, at);

		append (node, match);
	}

	match->list[2] = make_string (val);
}


void
set_property (struct node *node, char *name, char *val)
{
	int max_id, i;
	struct node *elt, *op, *id, *at, *elt_name, *match;
	
	if (node->type != LIST) {
		fprintf (stderr, "not a list");
		exit (1);
	}

	max_id = -1;
	match = NULL;
	for (i = 0; i < node->list_used; i++) {
		elt = node->list[i];

		if (elt->type != LIST)
			continue;
		if (elt->list_used < 3)
			continue;
		op = elt->list[0];
		if (op->type != SYMBOL)
			continue;
		if (strcmp (op->string, "property") != 0)
			continue;

		if ((id = alist_val (elt, "id")) != NULL
		    && id->type == INTEGER) {
			if (id->number > max_id)
				max_id = id->number;
		}
			
		elt_name = elt->list[1];

		if (elt_name->type != STRING)
			continue;
		if (strcmp (elt_name->string, name) != 0)
			continue;

		match = elt;
		break;
	}

	if (match == NULL) {
		match = make_list();
		append (match, make_symbol ("property"));
		append (match, make_string (name));
		append (match, NULL);

		id = make_list ();
		append (id, make_symbol ("id"));
		append (id, make_integer (max_id + 1));
		append (match, id);

		at = make_list ();
		append (at, make_symbol ("at"));
		append (at, make_float (50));
		append (at, make_float (60));
		append (match, at);

		append (node, match);
	}

	match->list[2] = make_string (val);
}

struct node *
make_sheet_node (struct sheet_info *sheet, char *inst_name)
{
	struct node *ret;
	struct node *elt;

	ret = make_list ();
	append (ret, make_symbol ("sheet"));
	
	elt = make_list ();
	append (elt, make_symbol ("at"));
	append (elt, make_float (50));
	append (elt, make_float (50));
	append (ret, elt);
		
	elt = make_list ();
	append (elt, make_symbol ("size"));
	append (elt, make_float (50));
	append (elt, make_float (50));
	append (ret, elt);
		
	elt = make_list ();
	append (elt, make_symbol ("uuid"));
	append (elt, make_uuid ());
	append (ret, elt);
	
	set_property (ret, "Sheet name", inst_name);
	set_property (ret, "Sheet file", sheet->filename);
	

	return (ret);
}



void
add_sheet (struct node *top, struct sheet_info *sheet, char *inst_name)
{
	struct node *sheet_node = find_sheet (top, sheet, inst_name);

	if (sheet_node == NULL) {
		sheet_node = make_sheet_node (sheet, inst_name);
		append (top, sheet_node);
	}
}

int
car_equal (struct node *elt, char *s)
{
	if (elt->type == LIST 
	    && elt->list_used > 0
	    && elt->list[0]->type == SYMBOL
	    && strcmp (elt->list[0]->string, s) == 0)
		return (1);
	return (0);
}

void
fixup_sheet_instances (struct node *top)
{
	for (top_idx = 0; top_idx < top->list_used; top_idx++) {
		elt = top->list[top_idx];
		if (car_equal (elt, "sheet")) {
			sheet_name = get_prop (elt, "Sheet name");
			sheet_file = get_prop (elt, "Sheet file");
			uuid = get_uuid (elt);
			sheet_used (sheet_name, sheet_file, uuid);
		}
	}

	struct node *si = assoc (top, "sheet_instances");

	if (si == NULL) {
		si = make_list();
		append (si, make_symbol ("sheet_instances"));
	}

	for (i = 1; i < si->list_used; i++) {
		elt = si->list[i];
		if (car_equal (elt, "path")) {
			
		}
	}
			

		

	
	

int
main (int argc, char **argv)
{
	int c;
	struct node *top;
	struct sheet_info *led;
	FILE *outf;

	srandom (time (NULL));
	
	while ((c = getopt (argc, argv, "")) != EOF) {
		switch (c) {
		default:
			usage ();
		}
	}

	top = make_empty ();

	led = get_sheet ("byhand/led.kicad_sch");
	add_sheet (top, led, "led1");
	add_sheet (top, led, "led2");

	fixup_sheet_instances (top);


	remove ("top.sch");
	outf = fopen ("top.sch", "w");
	print_exp (outf, top, 0);
	fclose (outf);

	
	return (0);
}

	
