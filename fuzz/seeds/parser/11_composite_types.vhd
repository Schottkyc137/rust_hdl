package p is
  type unbounded_t is array (natural range <>, integer range <>) of bit;
  type bounded_t is array (0 to 7) of bit;
  type rec_t is record
    a : bit;
    b, c : integer;
  end record rec_t;
  type acc_t is access rec_t;
  type file_t is file of character;
  type incomplete_t;
end package;
