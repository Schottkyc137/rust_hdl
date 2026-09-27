-- leading line comment
/* block
   comment */
entity e is -- trailing
  /* inline */ generic (g : integer); -- another
end e;
/**/
-- trailing line comment without newline
