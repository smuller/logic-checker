%token <string> UID
%token <string> LID
%token <int> INT
%token TRUE FALSE
%token AND OR COND BICOND NOT
%token IMP
%token FORALL EXISTS
%token LPAREN RPAREN COMMA DOT
%{ open Types
%}

%token NEWLINE EOF
%token TAB

%nonassoc FORALL EXISTS
%right COND BICOND
%left OR
%left AND
%nonassoc NOT
%nonassoc LPAREN RPAREN
%nonassoc IMP

%start proof
%type <Types.proof> proof

%%

proof:
  | prop IMP prop NEWLINE firstline NEWLINE lines { ($1, $3, $5::$7) }
  | error { raise (ParseError ("expected proof", $loc)) }
  | NEWLINE proof { $2 }

firstline:
  | line { $1 }
  | prop { ($startpos.pos_lnum, $1, Assumption) }
  | NEWLINE firstline { $2 }

lines:
  | EOF { [] }
  | NEWLINE lines { $2 }
  | line NEWLINE lines { $1::$3 }
  | line EOF { [$1] }

line:
  | prop TAB reason { ($startpos.pos_lnum, $1, $3) }
  | prop { raise (ParseError ("no justification", $loc)) }

reason:
  | { Assumption }
  | UID { App ($1, []) }
  | UID LPAREN RPAREN { App ($1, []) }
  | UID LPAREN exargs RPAREN { App ($1, $3) }
  | error { raise (ParseError ("invalid justification", $loc)) }

prop:
  | TRUE { True }
  | FALSE { False }
  | UID { Atom ($1, []) }
  | app { $1 }
  | NOT prop { Not $2 }
  | prop AND prop { And ($1, $3) }
  | prop OR prop { Or ($1, $3) }
  | prop COND prop { Cond ($1, $3) }
  | prop BICOND prop { Bicond ($1, $3) }
  | EXISTS LID DOT prop { Exists ($2, $4) }
  | FORALL LID DOT prop { Forall ($2, $4) }
  | LPAREN prop RPAREN { $2 }
  | error { raise (ParseError ("ill-formed prop", $loc)) }

app:
  | UID LPAREN RPAREN { Atom ($1, []) }
  | UID LPAREN vargs RPAREN { Atom ($1, $3) }

vargs:
  | LID { [$1] }
  | LID COMMA vargs { $1::$3 }

exargs:
  | INT { [$1] }
  | INT COMMA exargs { $1::$3 }
