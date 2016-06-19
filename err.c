/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: bpmn.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[20]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"STARTP",
	/* 03 */	"ENDP",
	/* 04 */	"CONN",
	/* 05 */	"FILECONN",
	/* 06 */	"CRIT",
	/* 07 */	"DIFFER",
	/* 08 */	"CORRECTF",
	/* 09 */	"FILEREAD",
	/* 10 */	"FILEWRITE",
	/* 11 */	"OPENP",
	/* 12 */	"CLOSEP",
	/* 13 */	"QUERIES",
	/* 14 */	"GPAR",
	/* 15 */	"GOR",
	/* 16 */	"GXOR",
	/* 17 */	"SEQ",
	/* 18 */	"ID",
	/* 19 */	"SPACE"
};
SetWordType zzerr1[4] = {0x0,0xc0,0x3,0x0};
SetWordType zzerr2[4] = {0x0,0x6,0x0,0x0};
SetWordType setwd1[20] = {0x0,0x1,0xca,0x20,0xca,0xca,0x0,
	0x0,0x0,0x0,0x0,0x0,0x0,0xcc,0x10,
	0x10,0x10,0x10,0x0,0x0};
SetWordType setwd2[20] = {0x0,0x1e,0x0,0x0,0x0,0x0,0x1d,
	0x1d,0x1d,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x0};
