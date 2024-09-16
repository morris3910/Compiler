%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
int yylex();
int max = 0,enter = 1,tmpnum = 0,tmp_size,tmp_isarray = 0,findit = 0;
char* tmp_id[256],tmp_t;
extern int charCount,lineCount,last_charCount,last_lineCount,correct;
char* output;
char* tmp;
char* tmp1;

void yyerror(const char* msg)
{
	correct=0;
	printf("\rLine %d, at char %d, ",last_lineCount,last_charCount);
	printf("%s                    \n",msg);
};

struct id_table
{
	char* id;
	char* type;
	int isarray;
	int arraysize;
}idtable[256];

void IDNotFound(char* str){
	correct=0;
	printf("\rLine %d, ",last_lineCount);
	printf("'%s' is not defined                    \n", str);
}

char* findID(char* str){

	for(int i = 0; i < strlen(str); i++) 
		str[i] = tolower(str[i]);
	for(int i = 0; i < max; i++) 
		if(!strcmp(idtable[i].id,str)) 
			return idtable[i].type;
	return "";
}

char* findexp(char* str){
	if(!strcmp(str,"diff_type")) 
		return "diff_type";
	for(int i = 0; i < strlen(str); i++) 
		if(str[i] == '.') 
			return "real";
	for(int i = 0; i < strlen(str); i++) 
		if(str[i] >= 48 && str[i] <= 57) 
			return "integer";
	for(int i = 0;i < strlen(str); i++) 
		str[i] = tolower(str[i]);
	for(int i = 0; i < max; i++) 
		if(!strcmp(idtable[i].id,str)) 
			return idtable[i].type;
	return "";
}

%}
%error-verbose
%union 
{
	char* name;
}
%type <name> prog prog_name dec_list dec type standtype arraytype id_list stmt_list stmt assign ifstmt exp relop simpexp term factor read write write_list for index_exp varid body error
%token <name> ABSOLUTE AND BBEGIN BREAK CASE CONST CONTINUE DO ELSE END FOR FUNCTION IF MOD NIL NOT OBJECT OF OR PROGRAM THEN TO VAR WHILE ARRAY INTEGER DOUBLE WRITE WRITELN STRING CHAR FLOAT READ REALTYPE //reserved word
%token <name> ASSIGNMENT COLON COMMA DOT EQUAL GE GT LBRAC LE LPAREN LT MINUS NOTEQUAL PLUS RBRAC RPAREN SEMICOLON SLASH STAR UPARROW //symbol
%token <name> INT REAL STR ID IOI IOR ROI ROR IDOI IDOR
%%
prog : PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BBEGIN stmt_list SEMICOLON END DOT
	{
		printf("\n");
	}
	| PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BBEGIN stmt_list SEMICOLON END
	{	
		correct=0;
		printf("\rLine %d, at char %d, ",last_lineCount,last_charCount+3);
		printf("syntax error, unexpected end, expecting .\n");
	}
	|error;
prog_name :  ID;
dec_list : dec 
	| dec_list SEMICOLON dec;
dec : id_list COLON type 
	| error;
type : standtype 
	{
		char* tmp = $1;
		for(int k=0; k < tmpnum; k++)
		{
			for(int j = 0; j < max; j++)
			{
				if(idtable[j].id == tmp_id[k])
				{
					enter=0;
					break;
				}
			}
			if(enter == 1)
			{
				if(tmp_isarray == 1)
				{
					idtable[max].isarray = 1;
					idtable[max].arraysize = tmp_size;
				}
				idtable[max].id = tmp_id[k];
				for(int l = 0; l < strlen(tmp); l++) 
					tmp[l] = tolower(tmp[l]);
				idtable[max++].type = $1;
			}
			else 
			{
				correct = 0;
				printf("\rLine %d, ", last_lineCount);
				printf("Duplicate identifier '%s'                    \n", tmp_id[k]);
				enter = 1;
				break;
			}
		}
		tmp_isarray = 0;
		tmpnum = 0;
	}
	| arraytype ;
standtype : INTEGER
	| REALTYPE
	| STRING
	| CHAR; 
arraytype : ARRAY LBRAC INT DOT DOT INT RBRAC OF standtype 
	{
		tmp_size = $6-$3-91;
		tmp_isarray = 1;
		char* tmp = $9;
		for(int k = 0; k < tmpnum; k++)
		{
			for(int j = 0; j < max; j++)
			{
				if(idtable[j].id == tmp_id[k])
				{
					enter = 0;
					break;
				}
			}
			if(enter == 1)
			{
				if(tmp_isarray == 1)
				{
					idtable[max].isarray = 1;
					idtable[max].arraysize = tmp_size;
				}
				idtable[max].id = tmp_id[k];
				for(int l = 0; l < strlen(tmp); l++) 
					tmp[l] = tolower(tmp[l]);
				idtable[max++].type = tmp;
			}
			else 
			{
				correct = 0;
				printf("\rLine %d, ", last_lineCount);
				printf("Duplicate identifier '%s'                    \n", tmp_id[k]);
				enter = 1;
				break;
			}
		}
		tmp_isarray=0;
		tmpnum=0;
	};
id_list : ID 
	{
		tmp_id[tmpnum++] = $1;
	}
	| id_list COMMA ID 
	{
		tmp_id[tmpnum++] = $3;
	}
	| error;
stmt_list : stmt 
	| stmt_list SEMICOLON stmt;
stmt : assign 
	| read 
	| write 
	| for 
	| ifstmt 
	| WRITELN;
assign : varid ASSIGNMENT simpexp 
	{
		if(findID($1)=="")
		{
			IDNotFound($1);
		}
		else if(findexp($3)=="")
		{
			IDNotFound($3);
		}
		else if(findexp($3)=="diff_type")
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",tmp,tmp1);
		}
		else if(strcmp(findID($1),findexp($3)) && strcmp(findexp($3),"diff_type"))
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findID($1));
		}
	}
	| varid relop simpexp 
	{
		correct=0;
		if(findID($1)=="")
		{
			IDNotFound($1);
		}
		else
		{
			printf("\rLine %d, ",last_lineCount);
			printf("syntax error, ':=' expected but '%s' found \n",$2);
		}
	}
	| varid ASSIGNMENT STR
	{
		if(findID($1)=="")
		{
			IDNotFound($1);
		}
		else if(strcmp(findID($1),"string")!=0)
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected 'string'\n",findID($1));
		}
	}
	| varid relop STR 
	{
		correct=0;
		if(findID($1)=="")
		{
			IDNotFound($1);
		}
		else
		{
			printf("\rLine %d, ",last_lineCount);
			printf("syntax error, ':=' expected but '%s' found \n",$2);
		}
	}
	| error;
ifstmt : IF LPAREN exp RPAREN THEN body;
exp : simpexp 
	{
		if(findexp($1)=="")
		{
			IDNotFound($1);
		}
	}
	| exp relop simpexp
	{
		if(findexp($1)=="")
		{
			IDNotFound($1);
		}
		else if(findexp($3)=="")
		{
			IDNotFound($3);
		}
		else if(strcmp(findexp($1),findexp($3))!=0 && strcmp(findexp($3),"diff_type")!=0)
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findexp($1));
		}
	};
relop : GT 
	| LT 
	| GE 
	| LE 
	| NOTEQUAL 
	| EQUAL;
simpexp : term
	| simpexp PLUS term 
	{
		tmp=strdup(findexp($3));
		tmp1=strdup(findexp($1));
		if(strcmp(findexp($1),findexp($3))!=0) 
			$$=strdup("diff_type");
		else
			$$=$1;
	}
	| simpexp MINUS term 
	{
		if(strcmp(findexp($1),findexp($3)) && strcmp(findexp($3),"diff_type"))
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findexp($1));$$=strdup("diff_type");
		}
		else if(!strcmp(findexp($3),"diff_type"))
			$$="diff_type";
		else 
			$$=$1;
	};
term : factor 
	| term STAR factor 
	{
		if(strcmp(findexp($1),findexp($3))!=0)
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findexp($1));$$=strdup("diff_type");
		}
		else 
			$$=$1;
	}
	| term SLASH factor 
	{
		if(strcmp(findexp($1),findexp($3))!=0)
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findexp($1));$$=strdup("diff_type");
		}
		else 
			$$=$1;
	}
	| term MOD factor 
	{
		if(strcmp(findexp($1),findexp($3))!=0)
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s', expected '%s'                    \n",findexp($3),findexp($1));$$=strdup("diff_type");
		}
		else 
			$$=$1;
	};
factor : varid 
	| INT 
	| REAL 
	| LPAREN simpexp RPAREN;
read : READ LPAREN read_list RPAREN;
read_list : varid 
	{
		if(findexp($1)=="")
		{
			IDNotFound($1);
		}
		tmp_isarray=0;
		tmpnum=0;
	}
	| STR 
	| read_list COMMA varid 
	{
		if(findexp($3)=="")
		{
			IDNotFound($3);
		}
		tmp_isarray=0;
		tmpnum=0;
	}
	| read_list COMMA STR;
write : WRITE LPAREN write_list RPAREN;
write_list : varid 
	{
		if(findexp($1)=="")
		{
			IDNotFound($1);
		}
		tmp_isarray=0;
		tmpnum=0;
	}
	| STR 
	| write_list COMMA varid 
	{
		if(findexp($3)=="")
		{
			IDNotFound($3);
		}
		tmp_isarray=0;
		tmpnum=0;
	}
	| write_list COMMA STR;
for : FOR index_exp DO body;
index_exp : varid ASSIGNMENT simpexp TO exp;
varid : ID
	| ID LBRAC simpexp RBRAC
	{
		if(findexp($3)=="")
		{
			IDNotFound($3);
		}
		else if(strcmp(findexp($3),"integer"))
		{
			correct=0;
			printf("\rLine %d, ",last_lineCount);
			printf("Incompatible types: got '%s'                     ,expected 'integer'\n",findexp($3));
		}
	};
body : stmt 
	| BBEGIN stmt_list SEMICOLON END;
%%
int main()
{
	yyparse();
	return 0;
}
