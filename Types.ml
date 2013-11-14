type instruction = 
  | Assign of lvalue * rvalue 
  | Branch of taken * id * condition  

and taken = bool                            

and id = int

and lvalue = address * (value option) 

and rvalue = 
  | Bop of bop * lvalue * lvalue
  | Neg of lvalue
  | Lvalue of lvalue
  | Symbvalue of typ * symvar

and symvar = string

and condition = 
  | Const of bool 
  | Cond of rop * lvalue * lvalue

and bop = Plus | Minus | Mult | Unintepreted

and rop = LT | LE | GT | GE | EQ | NEQ 

and typ = 
  | Integer | UInt | Char | UChar  
  | Short | UShort | Long | ULong
  | LLong | ULLong 

and address = int

and value = int
