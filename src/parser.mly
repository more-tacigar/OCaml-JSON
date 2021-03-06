%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COLON
%token COMMA
%token EOF

%start <Syntax.value option> program

%%

program
  : v = value; EOF { Some v }
  ;

value
  : LEFT_BRACE; obj = object_fields; RIGHT_BRACE	{ `Assoc obj }
  | LEFT_BRACKET; vl = array_values; RIGHT_BRACKET	{ `List vl }
  | s = STRING						{ `String s }
  | i = INT						{ `Int i }
  | x = FLOAT						{ `Float x }
  | TRUE						{ `Bool true }
  | FALSE						{ `Bool false }
  | NULL						{ `Null }
  ;

object_fields
  : rof = rev_object_fields { List.rev rof }
  ;

rev_object_fields
  : obj = separated_list(COMMA, object_field) { obj }
  ;

object_field
  : k = STRING; COLON; v = value { (k, v) }
  ;

array_values
  : vl = separated_list(COMMA, value) { vl }
  ;

