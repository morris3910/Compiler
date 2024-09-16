%{
#include "y.tab.h"
#include <stdio.h>
unsigned int lineCount=1,charCount=1,last_charCount=1,last_lineCount=1,correct=1;
%}
%x check_operator
letter [a-zA-Z]
digit [0-9]
eol \r\n|\n
id [a-zA-Z_][a-zA-Z0-9_]{0,14}
integer [+-]?0|[+-]?[1-9][0-9]*
fraction [0-9][1-9]*
string [']([^'\n\r\n]|[']['][^'\n\r\n]*)*[']
comment [(][*]([^*]|{eol}|([*]+([^*)]|{eol})))*[*]+[)]{eol}*
real ({integer}|{integer}"."{fraction})([Ee]{integer})?
%%
<*>[Aa][Bb][Ss][Oo][Ll][Uu][Tt][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return ABSOLUTE;}
<*>[Aa][Nn][Dd] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return AND;}
<*>[Bb][Ee][Gg][Ii][Nn] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return BBEGIN;}
<*>[Bb][Rr][Ee][Aa][Kk] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return BREAK;}
<*>[Cc][Aa][Ss][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return CASE;}
<*>[Cc][Oo][Nn][Ss][Tt] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return CONST;}
<*>[Cc][Hh][Aa][Rr] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return CHAR;}
<*>[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return CONTINUE;}
<*>[Dd][Oo] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return DO;}
<*>[Ee][Ll][Ss][Ee]  {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return ELSE;}
<*>[Ee][Nn][Dd] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return END;}
<*>[Ff][Oo][Rr] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return FOR;}
<*>[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return FUNCTION;}
<*>[Ii][Ff] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return IF;}
<*>[Mm][Oo][Dd] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return MOD;}
<*>[Nn][Ii][Ll] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return NIL;}
<*>[Nn][Oo][Tt] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return NOT;}
<*>[Oo][Bb][Jj][Ee][Cc][Tt] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return OBJECT;}
<*>[Oo][Ff] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return OF;}
<*>[Oo][Rr] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return OR;}
<*>[Pp][Rr][Oo][Gg][Rr][Aa][Mm] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return PROGRAM;}
<*>[Tt][Hh][Ee][Nn] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return THEN;}
<*>[Tt][Oo] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return TO;}
<*>[Vv][Aa][Rr] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return VAR;}
<*>[Ww][Hh][Ii][Ll][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return WHILE;}
<*>[Aa][Rr][Rr][Aa][Yy] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return ARRAY;}
<*>[Ii][Nn][Tt][Ee][Gg][Ee][Rr] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return INTEGER;}
<*>[Dd][Oo][Uu][Bb][Ll][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return DOUBLE;}
<*>[Ww][Rr][Ii][Tt][Ee] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return WRITE;}
<*>[Ww][Rr][Ii][Tt][Ee][Ll][Nn] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return WRITELN;}
<*>[Ss][Tt][Rr][Ii][Nn][Gg] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return STRING;}
<*>[Ff][Ll][Oo][Aa][Tt] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return FLOAT;}
<*>[Rr][Ee][Aa][Dd] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return READ;} 
<*>[Rr][Ee][Aa][Ll] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return REALTYPE;} 
<*>[ ] {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1) printf(" ");charCount++;} 
<*>[ ]*{eol} {if (charCount!=1&&correct == 1) printf("\n");correct=1;charCount=1;lineCount++;} 
{integer} {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN check_operator;return INT;} 
{real} {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN check_operator;return REAL;} 
{string} {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return STR;}	
<*>\:\=    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return ASSIGNMENT;} 
<*>\:    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return COLON;}
<*>\,    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return COMMA;}
<*>\.   {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return DOT;}
<*>\=    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return EQUAL;}
<*>\>\=    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return GE;}
<*>\>   {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return GT;}
<*>\[    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return LBRAC;}
<*>\<\=    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return LE;}
<*>\(    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return LPAREN;}
<*>\<    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return LT;}
<check_operator>\-   {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return MINUS;}
<*>\<\>    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return NOTEQUAL;}
<check_operator>\+    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return PLUS;}
<*>\]    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return RBRAC;}
<*>\)    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return RPAREN;}
<*>\;    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return SEMICOLON;}
<*>\/    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return SLASH;}
<*>\*    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return STAR;}
<*>\^    {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN INITIAL;return UPARROW;}	
<*>{id} {if (charCount == 1 && correct == 1) printf("Line %d: ", lineCount);if (correct == 1){printf("%s", yytext);}last_charCount = charCount;last_lineCount = lineCount;charCount += yyleng;yylval.name = strdup(yytext);BEGIN check_operator;return ID;}
<*>[ ]*{comment} {
	for (int i=0; i<yyleng; i++) {if (yytext[i] == 10){lineCount++; charCount=1;} else charCount++;}BEGIN INITIAL;
}	//comment
%%

