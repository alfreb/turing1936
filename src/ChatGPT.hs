

type Tape = String
type Action = Char
type MFunctionRef = Int

type MFunction = Tape -> Char -> Char -> [(Char -> Bool, [Action], MFunctionRef)]

f :: MFunction
f tape c b a =
  [ ((=='ә'), [ _L ], mRef f1 (c,b,a))
  , ((/='ә'), [ _L ], mRef self)
  ]

f1 :: MFunction
f1 tape c b a =
  [ ((== a),    [    ], mRef c)
  , ((/= a),    [ _R ], mRef f1 (c,b,a))
  , ((== none), [ _R ], mRef f2 (c,b,a))
  ]

f2 :: MFunction
f2 tape c b a =
  [ ((== a),    [    ], mRef c)
  , ((/= a),    [ _R ], mRef f1 (c,b,a))
  , ((== none), [ _R ], mRef b)
  ]

