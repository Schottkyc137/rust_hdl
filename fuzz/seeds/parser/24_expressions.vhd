architecture a of e is
  constant c1 : boolean := a and b and c;
  constant c2 : boolean := a or b nand c nor d xor e xnor f;
  constant c3 : boolean := not a;
  constant c4 : integer := -a + +b - c;
  constant c5 : integer := a * b / c mod d rem e;
  constant c6 : integer := a ** b ** c;
  constant c7 : integer := abs a;
  constant c8 : bit_vector := a & b & c;
  constant c9 : boolean := a = b and a /= b and a < b and a <= b and a > b and a >= b;
  constant c10 : bit := a ?= b or a ?/= b or a ?< b or a ?<= b or a ?> b or a ?>= b;
  constant c11 : bit := ?? a;
  constant c12 : integer := a sll 1 srl 2 sla 3 sra 4 rol 5 ror 6;
  constant c13 : integer := (((a)));
  constant c14 : boolean := a when b else c;
begin
end architecture;
