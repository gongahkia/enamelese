abstract Enamel = {

  flags startcat = Program ;

  cat
    Program ;
    Statement ;
    Expression ;
    Condition ;
    Loop ;
    Function ;
    Class ;
    DataType ;
    Operator ;
    VillagerName ;
    Value ;
    ErrorHandler ;

  fun
    -- Program structure
    StartProgram : Program ;  -- 🏝️
    EndProgram : Program ;    -- 🛫
    AddStatement : Statement -> Program -> Program ;

    -- Comments
    Comment : String -> Statement ;  -- 🎣

    -- Statements
    DeclareVariable : VillagerName -> String -> Value -> Statement ;  -- [name] moves in: [value]
    AssignVariable : VillagerName -> String -> Value -> Statement ;   -- [name] learns: [value]
    PrintStatement : VillagerName -> String -> Value -> Statement ;   -- [name] says: [value]
    InputStatement : VillagerName -> String -> Statement ;            -- [name] listens:
    
    -- Control flow
    IfStatement : Condition -> Statement -> Statement -> Statement ;  -- 🤔 ... 🙃 ... 😌
    LoopStatement : Loop -> Statement ;                               -- 🏃‍♂️ ... 😴
    
    -- Functions
    FunctionDef : String -> [VillagerName] -> Statement -> Function ; -- 🎁 ... 🎀
    FunctionCall : String -> [Expression] -> Expression ;             -- 🔔[function]([args])🔔
    
    -- Classes
    ClassDef : String -> [Function] -> Class ;  -- 🏠 ... 🏠
    
    -- Expressions
    StringExpr : String -> Expression ;    -- 💬...💬
    NumberExpr : Int -> Expression ;       -- 🔔...🔔
    BooleanExpr : Bool -> Expression ;     -- 🦉 or 🦝
    ListExpr : [Expression] -> Expression ;  -- 🌴[...]🌴
    DictExpr : [(String, Expression)] -> Expression ;  -- 🏠{...}🏠
    
    -- Operators
    ArithmeticOp : Operator -> Expression -> Expression -> Expression ;
    ComparisonOp : Operator -> Expression -> Expression -> Expression ;
    LogicalOp : Operator -> Expression -> Expression -> Expression ;
    
    -- Operators
    Add : Operator ;         -- 🍎
    Subtract : Operator ;    -- 🍐
    Multiply : Operator ;    -- 🍊
    Divide : Operator ;      -- 🍑
    Modulo : Operator ;      -- 🥥
    Equal : Operator ;       -- 🐠
    NotEqual : Operator ;    -- 🦈
    GreaterThan : Operator ; -- 🐙
    LessThan : Operator ;    -- 🦀
    And : Operator ;         -- 🦋
    Or : Operator ;          -- 🐝
    Not : Operator ;         -- 🐞
    
    -- Error handling
    TryCatch : Statement -> ErrorHandler -> Statement ;  -- 🎭 ... 🃏 ... 🎭

  -- Data types
  StringType, NumberType, BooleanType, ListType, DictType : DataType ;

}