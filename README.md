# Turing1936
Turing machines implemented directly from Turing's 1936 paper titled
*ON COMPUTABLE NUMBERS, WITH AN APPLICATION TO THE ENTSCHEIDUNGSPROBLEM*.

The intention is to provide an implementation that's as close to the original as possible, in order to make it easy to verify that they really are Turing machines and not second hand representations. Conventions specific to the implementation language are some times sacrificed for this obejctive.

The source used is a digital reprint of the paper found in The Turing Digital Archive:
https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-12


## Original machine examples

Turing's first example machine looks like this in the paper: 
![Turing's original first example machine](Turing1936_Example1_Original.png)

It looks something like this in code:
```
  [
    (ð, (==none), [    _P0    ],  ð),
    (ð, (=='0' ), [_R, _R, _P1],  ð),
    (ð, (=='1' ), [_R, _R, _P0],  ð)
  ]

```

His second example looks like this in the paper: 
![Turing's original second example machine](Turing1936_Example2_Original.png)

It looks something like this in code:
```
  [
    (ð, (\x -> True), [_PÓ, _R, _PÓ, _R, _P0, _R, _R, _P0, _L, _L  ], ð¬),
    (ð¬, (=='1' ),     [             _R, _Px, _L, _L, _L            ], ð¬),
    (ð¬, (=='0' ),     [                                            ], ð®),
    (ð®, (`elem` "01"),[                    _R, _R                  ], ð®),
    (ð®, (==none),     [                   _P1, _L                  ], ð­),
    (ð­, (=='x'),      [                    _E, _R                  ], ð®),
    (ð­, (=='Ó'),      [                      _R                    ], ð£),
    (ð­, (==none),     [                    _L, _L                  ], ð­),
    (ð£, (/= none),    [                    _R, _R                  ], ð£),
    (ð£, (==none),     [                   _P0,_L,_L                ], ð¬)
  ]
```

The paper itself and reprints provided below are copyright protected:
Copyright Â© London Mathematical Society 1937

I have applied to the copyright holder for the right to reproduce pictures of the example machines above, approval pending.
