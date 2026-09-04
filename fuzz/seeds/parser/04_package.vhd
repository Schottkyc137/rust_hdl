package p is
  type t is (a, b, c);
  function f (x : integer) return integer;
end package p;

package body p is
  function f (x : integer) return integer is
  begin
    return x;
  end function f;
end package body p;
