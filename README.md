# Advanced-Programming (Haskell & Erlang)

This is the set of assignments for UCPH Advanced Programming course 2022. The main purpose of the project is to manage assignments. If some classmates are lucky enough to find our project on GitHub, please do not completely copy our code. Of course, you are welcome to refer to our ideas.

## Completion

### Assignment 1

| Class of Function                 | Function Name | Completion (○/△/×) |
| --------------------------------- | ------------- |:------------------ |
| Printing expressions              | showExp       | ○                  |
| Evaluating expressions            | evalSimple    | ○                  |
| Extended arithmetic expressions   | extendEnv     | ○                  |
| Extended arithmetic expressions   | evalFull      | ○                  |
| Returning explicit errors         | evalErr       | ○                  |
| Printing with minimal parentheses | showCompact   | ×                  |
| Explicitly eager/lazy semantics   | evalEager     | ×                  |
| Explicitly eager/lazy semantics   | evalLazy      | ×                  |

**Note:** The last three functions are optional and we haven't done it.

### Assignment 2

| Class of Function     | Function Name | Completion (○/△/×) |
| --------------------- | ------------- |:------------------ |
| Monad operations      | abort         | ○                  |
| Monad operations      | look          | ○                  |
| Monad operations      | withBinding   | ○                  |
| Monad operations      | output        | ○                  |
| Auxilary functions    | truthy        | ○                  |
| Auxilary functions    | operate       | ○                  |
| Auxilary functions    | apply         | ○                  |
| Interpreter functions | eval          | △                  |
| Interpreter functions | exec          | ○                  |
| Interpreter functions | execute       | ○                  |

**Note:** There is a problem with the Compr pattern matching in the eval function and it does not fully pass OnlineTA

### Assignment 3

| Class of Function   | Function Name   | Completion (○/△/×) |
| ------------------- | --------------- |:------------------ |
| main function       | parseString     | ○                  |
| Program             | rpProgram       | ○                  |
| Stmts               | rpStmts         | ○                  |
| Stmt                | rpStmt          | ○                  |
| Expr                | operExp         | △                  |
| ident               | rpIdent         | ○                  |
| numConst            | concreteOperExp | ○                  |
| stringConst         | concreteOperExp | ○                  |
| Auxiliary functions | token           | ○                  |
| Auxiliary functions | extractIdent    | ○                  |
| Auxiliary functions | rpComment       | ○                  |

**Note:**  There is a problem with the parsing of deep brackets, etc., and 7 timeouts appear in OnlineTA. In addition, the parsing of the parentheses directly after the keyword is different from the requirements of OnlineTA. The specific reason for the error is that our program specifies that the keyword must be separated by a space, e.g. **"notx"** would not be recognized as **Not (Var "x")** but as **Var "notx"** as a **Var**. however, it happens when the keywords immediately followed by parentheses(**"()"** or **"[]"**). OnlineTA tells us that **"not(x)"** should be correctly recognized as **Not ( Var "x")**, but since our program need a space after the keyword, such a situation is not allowed, and the program will return an error.
