#header
<<
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
>>
 
<<
#include <cstdlib>
#include <cmath>
 
//global structures
AST *root;
 
 
// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
/*  if (type == ID) {
    attr->kind = "id";
    attr->text = text;
  }
  else {*/
    attr->kind = text;
    attr->text = "";
//  }
}
 
// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind;
  as->text = attr->text;
  as->right = NULL;
  as->down = NULL;
  return as;
}
 
 
/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}
 
/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}
 
 
 
/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;
 
  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;
 
  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
 
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}
 
/// print AST
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

AST *find(string role) {
  AST *a = root->down->down;
  while (a != NULL) {
    if (a->kind == role) return a;
    a=a->right;
  }  
  return NULL;
}

bool operador0(string role){
  return (role == "+" || role == "|" || role == "#");
}
bool operador1(string role){
  return (role == ";");
}

int calc_critical(AST *a) {
  int ans = 1;
  if (operador0(a->kind))     ans = max(calc_critical(a->down), calc_critical(a->down->right));
  else if (operador1(a->kind))ans = calc_critical(a->down) + calc_critical(a->down->right);
  return ans;
}

int critical(string role) {
  AST *i = find(role);
  if (i == NULL) return -1;
  i = i->down;
  return calc_critical(i);
}

bool operador(string role){
  return (role == "+" || role == "|" || role == "#" || role == ";");
}

bool calc_difference(AST *a1, AST *a2) {
  if (a1 == NULL && a2 == NULL)                                 return false;
  if ((a1 == NULL && a2 != NULL) || (a2 == NULL && a1 != NULL)) return true;
  if (operador(a1->kind)){  
    if(a1->kind != a2->kind)                                    return true;                    
    return calc_difference(a1->down,a2->down) || calc_difference(a1->down->right,a2->down->right);
  }
  return false;
}

bool difference(string role1, string role2) {  
  AST *i  = find(role1); 
  if (i != NULL)   i = i->down;
  AST *i2 = find(role2); 
  if (i2 != NULL) i2 = i2->down;
  return    calc_difference(i,i2);     
}

void recorre(AST *a) {
  while (a!=NULL) {
    if (a->kind == "critical") {
      cout << "Critical " << child(a,0)->kind << " : " << critical(child(a,0)->kind) << endl;
    }
    else if (a->kind == "difference") {
      cout << "Difference: " << child(a,0)->kind << " and " << child(a,1)->kind<< " : " << difference(child(a,0)->kind,child(a,1)->kind) << endl;
    }
    a=a->right;
  }  
}
 

int main() {
  root = NULL;
  ANTLR(bpmn(&root), stdin);
  ASTPrint(root);
  recorre(child(child(root,1),0));
}
>>
 
#lexclass START
#token STARTP "start"
#token ENDP "end"
#token CONN "connection"
#token FILECONN "file"
#token CRIT "critical"
#token DIFFER "difference"
#token CORRECTF "correctfile"
#token FILEREAD "\->"
#token FILEWRITE "<\-"
#token OPENP "\("
#token CLOSEP "\)"
#token QUERIES "queries"
#token GPAR "\+"
#token GOR "\|"
#token GXOR "\#"
#token SEQ ";"
#token ID "[a-zA-Z][a-zA-Z0-9]*"
#token SPACE "[\ \n]" << zzskip();>>


///--------------------------------------------------------------------
///--------------------------------------------------------------------
///--------------------------------------------------------------------

bpmn: process QUERIES! queries <<#0=createASTlist(_sibling);>>;
 
///Begin of process operations:
process: 	(start|conex|file)* <<#0=createASTlist(_sibling);>>;

start: 		 STARTP! op0 ENDP! ID^;
op0: 		 op1 (GPAR^ op1)*;
op1: 		 op2 (GXOR^ op2)*;
op2: 		 op3 (GOR^ op3)*;
op3: 		 contingut (SEQ^ contingut)*;
contingut: 	 (ID|(OPENP! op0 CLOSEP!));  

file: 		 FILECONN^ fileaux;
fileaux:	 ID (FILEREAD^|FILEWRITE^) ID;
conex: 		 CONN^ ID ID;

///--------------------------------------------------------------------
///--------------------------------------------------------------------
///--------------------------------------------------------------------

///Begin of queries operations:
queries: 	 (crit|dif|correctf)* <<#0=createASTlist(_sibling);>>;

crit: 		 CRIT^ ID;
dif: 		 DIFFER^ ID ID;
correctf:    CORRECTF^ ID;


///Ricard Meyerhofer Parra, Grup 12, Practica Compiladors.