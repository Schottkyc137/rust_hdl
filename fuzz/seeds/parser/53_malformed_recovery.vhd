context is
end context;

package p is
  type r is record
    ,
    a : bit;
    b, : integer;
  end record;
  attribute a1 of 1 : signal is 2;
  attribute a2 of foo, , bar : signal is 3;
end package;

entity e is
  generic (
    x is <>;
    procedure
  );
end entity;
