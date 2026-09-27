package body p is
  procedure prc (
    signal   s : out bit;
    variable v : inout integer;
    constant k : in  integer := 0;
    file     f : text
  ) is
  begin
  end procedure prc;

  pure function pf return integer is
  begin
    return 0;
  end;

  impure function "and" (l, r : bit) return bit is
  begin
    return l;
  end function "and";

  function gf
    generic (type t; n : natural)
    parameter (x : t)
    return t is
  begin
    return x;
  end function;

  alias a is prc [bit, integer, integer, text];
end package body;
