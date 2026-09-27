entity e is
  generic (
    g1 : integer := 0;
    g2 : string
  );
  port (
    clk : in bit;
    d   : in bit_vector(7 downto 0);
    q   : out bit_vector(7 downto 0);
    b   : buffer bit;
    io  : inout bit
  );
  constant c : integer := 1;
begin
  assert g1 >= 0 report "bad" severity failure;
end entity;
