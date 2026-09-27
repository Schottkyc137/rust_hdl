architecture a of e is
  constant c : integer := 0;
  signal s1, s2 : bit := '0';
  signal reg : bit register;
  signal bs  : bit bus;
  shared variable sv : pt;
  file f : text open read_mode is "name.txt";
  alias al is <<signal .top.s : bit>>;
  alias fn is "+" [integer, integer return integer];
  attribute attr : string;
  attribute attr of s1 : signal is "v";
  disconnect s1 : bit after 1 ns;
  disconnect all : bit after 1 ns;
  disconnect others : bit after 1 ns;
  group gt is (label, signal <>);
  group g : gt (s1, s2);
  use work.p.all;
begin
end architecture;
