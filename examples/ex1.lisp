
(TEN/TEMPLATE:TEMPLATE TEN/EXAMPLES::EX1 NIL
                       (TEN/EXAMPLES::FOO TEN/EXAMPLES::BAR &KEY LIST)
                       (WRITE-STRING "
<html>
  <head>
  </head>
  <body>
    "
                                     . #1=(TEN/TEMPLATE:%TEN-STREAM))
                       (WRITE-STRING
                        (TEN/TEMPLATE:ESC
                         (WITH-OUTPUT-TO-STRING (#2=#:STRING-STREAM558)
                           (PRINC TEN/EXAMPLES::FOO #2#)))
                        . #1#)
                       (WRITE-STRING "

    "
                                     . #1#)
                       (IF TEN/EXAMPLES::BAR
                           (PROGN
                            (WRITE-STRING "
    True
    "
                                          . #1#))
                           (PROGN
                            (WRITE-STRING "
    False
    "
                                          . #1#)))
                       (WRITE-STRING "

    "
                                     . #1#)
                       (WHEN LIST
                         (WRITE-STRING "
    <ul>
      "
                                       . #1#)
                         (LOOP TEN/EXAMPLES::FOR TEN/EXAMPLES::ITEM TEN/EXAMPLES::IN LIST
                               DO (WRITE-STRING "
      <li>"
                                                . #1#) (WRITE-STRING
                                                        (TEN/TEMPLATE:ESC
                                                         (WITH-OUTPUT-TO-STRING
                                                             (#3=#:STRING-STREAM559)
                                                           (PRINC
                                                            TEN/EXAMPLES::ITEM
                                                            #3#)))
                                                        . #1#) (WRITE-STRING
                                                                "</li>
      "
                                                                . #1#))
                         (WRITE-STRING "
    </ul>
    "
                                       . #1#))
                       (WRITE-STRING "

    "
                                     . #1#)
                       (WHEN (NOT LIST)
                         (WRITE-STRING "
    There's no list
    "
                                       . #1#))
                       (WRITE-STRING "
    

    "
                                     . #1#)
                       (WRITE-STRING
                        (TEN/TEMPLATE:ESC
                         (WITH-OUTPUT-TO-STRING (#4=#:STRING-STREAM560)
                           (PRINC (PRINC-TO-STRING TEN/EXAMPLES::BAR) #4#)))
                        . #1#)
                       (WRITE-STRING "

    "
                                     . #1#)
                       (TEN/TEMPLATE:RAW
                         (WRITE-STRING "
    This goes unescaped
    "
                                       . #1#))
                       (WRITE-STRING "
  </body>
</html>
"
                                     . #1#)) 