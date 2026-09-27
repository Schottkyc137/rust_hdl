package p is
  type pt is protected
    procedure set (x : integer);
    impure function get return integer;
  end protected pt;
end package;

package body p is
  type pt is protected body
    variable v : integer := 0;
    procedure set (x : integer) is
    begin
      v := x;
    end procedure;
    impure function get return integer is
    begin
      return v;
    end function;
  end protected body pt;
end package body;
