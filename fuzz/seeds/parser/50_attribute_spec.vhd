package p is
  attribute a1 of all : signal is 1;
  attribute a2 of others : variable is 2;
  attribute a3 of foo [return integer] : function is 3;
  attribute a4 of "and" [bit, bit return bit] : function is 4;
  attribute a5 of 'x' [] : literal is 5;
  attribute a6 of foo, bar : type is 6;
end package;
