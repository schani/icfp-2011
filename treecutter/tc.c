
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <getopt.h>
#include <alloca.h>
#include <ctype.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdint.h>
#include <stdbool.h>
#include <math.h>


#define	MIN(a,b)	(((a) < (b)) ? (a) : (b))
#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))

#define	CLAMP(v,a,b)	MIN(b, MAX(a,v))



typedef	struct _node {
	char *val;
	struct _node *left;
	struct _node *right;
	unsigned num;
}	node_t;



node_t * split_expr(char *e, unsigned l) {
	int i, depth = 0;

	// printf("split %p, %d, >%.*s<\n", e, l, l, e);
	
	for (i=0; i<l; i++) {
		switch (e[i]) {
		case '(':
			depth++;
			break;
		case ')':
			depth--;
			break;
		case ' ':
			if (depth == 1)
				goto found;
			break;
		}
	}
	
	node_t *leaf = calloc(1, sizeof(node_t));
	leaf->val = e;
	e[l] = '\0';
	return leaf;
	
found:	;
	node_t *n = calloc(1, sizeof(node_t));
	n->left = split_expr(e + 1, i - 1);
	n->right = split_expr(e + i + 1, l - i - 2);
	return n;
}




void	dump_node(node_t *n) {

	if (n->val) {
		printf("%s", n->val);
	} else {
		printf("(");
		dump_node(n->left);
		printf(" ");
		dump_node(n->right);
		printf(")");
	}
}


unsigned leaf_num;

void	enumerate_leafs(node_t *n) {
	if (n->val) {
		n->num = leaf_num++;
		return;
	}
	enumerate_leafs(n->left);
	enumerate_leafs(n->right);
}

node_t * find_leaf_parent(node_t *n, unsigned num) {
	if (n->val)
		return NULL;

	if (n->left->num == num || n->right->num == num)
		return n;

	node_t *r = find_leaf_parent(n->left, num);
	if (r)
		return r;

	return find_leaf_parent(n->right, num);
}



unsigned node_num;

void	enumerate_nodes(node_t *n) {
	n->num = node_num++;
	if (n->val)
		return;

	enumerate_nodes(n->left);
	enumerate_nodes(n->right);
}

node_t * find_node(node_t *n, unsigned num) {
	if (n->num == num)
		return n;

	if (n->val)
		return NULL;

	node_t *r = find_node(n->left, num);
	if (r)
		return r;

	return find_node(n->right, num);
}




bool	contains_node(node_t *n, node_t *s) {
	if (n == s)
		return true;
	if (n->val)
		return false;
	
	if (contains_node(n->left, s))
		return true;
	return contains_node(n->right, s);
}

void	all_subtrees_with(node_t *n, node_t *s) {
	if (n->val)
		return;
	
	// printf("subtrees %p with %p\n", n, s);
	if (contains_node(n->left, s)) {
		printf("@ ");
		dump_node(n->left);
		printf("\n");
		all_subtrees_with(n->left, s);
	}
	if (contains_node(n->right, s)) {
		printf("@ ");
		dump_node(n->right);
		printf("\n");
		all_subtrees_with(n->right, s);
	}
}




node_t	dummy = { .val = "X" };


typedef	struct _pattern {
	node_t *pat;
	unsigned count;
	unsigned num;
}	pattern_t;




pattern_t *pattern;
unsigned pattern_max = 0;
unsigned pattern_num = 0;



bool	match_pattern(node_t *a, node_t *b, bool exact)
{
	if (a == &dummy) {
		if (!exact)
			return true;
		return (b == &dummy);
	}
	if (a->val)
		return (b->val &&
			(strcmp(a->val, b->val) == 0));
	if (b->val)
		return false;
	
	bool t = match_pattern(a->left, b->left, exact);
	if (!t)
		return false;
	return match_pattern(a->right, b->right, exact);
}


node_t * copy_node(node_t *n) {
	if (!n)
		return NULL;
	
	if (n == &dummy)
		return &dummy;
	
	node_t *c = calloc(1, sizeof(node_t));
	
	c->val = n->val;
	c->left = copy_node(n->left);
	c->right = copy_node(n->right);
	return c;
}


void	save_pattern(node_t *n) {
	
	if (pattern_num == pattern_max - 1) {
		pattern_max *= 2;
		pattern = realloc(pattern, sizeof(pattern_t) * pattern_max);
	}
	
	pattern[pattern_num].pat = copy_node(n);
	pattern[pattern_num].count = 0;
	pattern_num++;
}

bool	pattern_exists(node_t *n) {
	for (int i=0; i<pattern_num; i++)
		if (match_pattern(n, pattern[i].pat, true))
			return true;
	return false;
}




void	dump_pattern(pattern_t *p) {
	printf("# %5d\t", p->count);
	dump_node(p->pat);
	printf("\n");
}

void	dump_all_pattern(void) {
	for (int i=0; i<pattern_num; i++)
		dump_pattern(&pattern[i]);
}

void	count_pattern(pattern_t *p, node_t *n) {
	if (match_pattern(p->pat, n, false))
		p->count++;
	if (n->val)
		return;
	count_pattern(p, n->left);
	count_pattern(p, n->right);
}

void	count_all_pattern(node_t *n) {
	for (int i=0; i<pattern_num; i++)
		count_pattern(&pattern[i], n);
}




void	all_trees_with(node_t *n, node_t *s) {
	if (n->val)
		return;
	
	// printf("trees %p with %p\n", n, s);
	if (contains_node(n, s)) {
		printf("& ");
		dump_node(n);
		if (!pattern_exists(n))
			save_pattern(n);
		printf("\n");
		all_trees_with(n->left, s);
		all_trees_with(n->right, s);
	}
}



void	cut_leafs(node_t *n) {
	leaf_num = 1;
	enumerate_leafs(n);
	unsigned ln = leaf_num;
	
	// printf("leaf %p count %d\n", n, leaf_num);
	for (int i=1; i<ln; i++) {
		node_t *lp = find_leaf_parent(n, i);
		
		// printf("leaf %p cut %d with lp %p\n", n, i, lp);
		if (!lp)
			continue;

/*		if (n == lp)
			return;	*/

		node_t *t = lp->left;
		lp->left = &dummy;
		all_subtrees_with(n, lp);
		lp->left = t;

		t = lp->right;
		lp->right = &dummy;
		all_subtrees_with(n, lp);
		lp->right = t;
	}
}

void	cut_branches(node_t *n) {
	node_num = 1;
	enumerate_nodes(n);
	unsigned nn = node_num;
	
	// printf("branch %p count %d\n", n, nn);
	for (int i=1; i<nn; i++) {
		node_t *c = find_node(n, i);
		
		// printf("branch %p cut %d with %p\n", n, i, c);
		if (!c)
			continue;

		node_t *t = c->left;
		c->left = &dummy;
		all_trees_with(n, &dummy);
		c->left = t;

		t = c->right;
		c->right = &dummy;
		all_trees_with(n, &dummy);
		c->right = t;
	}
}


void	cut_tree(node_t *n, node_t *o) {
	if (n->val)
		return;
	
	node_t *t;
		
	cut_tree(n->left, o);
	t = n->left;
	n->left = &dummy;
	all_trees_with(o, &dummy);
	n->left = t;
		
	cut_tree(n->right, o);
	t = n->right;
	n->right = &dummy;
	all_trees_with(o, &dummy);
	n->right = t;
}




void	all_subtrees(node_t *n) {
	if (!n->val) {
		if (!n->left->val) {
			printf("# ");
			dump_node(n->left);
			printf("\n");
			all_subtrees(n->left);
			cut_branches(n->left);
		}
		if (!n->right->val) {
			printf("# ");
			dump_node(n->right);
			printf("\n");
			all_subtrees(n->right);
			cut_branches(n->right);
		}
	}
}


int	main(int argc, char *argv[]) {
	size_t length;

	int fd = open(argv[1], O_RDONLY);
	length = lseek(fd, 0, SEEK_END);
	char *data = mmap(NULL, length, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);

	while (data[length - 1] != ')')
		length--;

	node_t *top = split_expr(data, length);

	pattern = calloc(256, sizeof(pattern_t));
	pattern_max = 256; 


	// dump_node(top);
	
	cut_tree(top, top);
	// all_subtrees(top);

	count_all_pattern(top);
	dump_all_pattern();

	exit(0);
}

