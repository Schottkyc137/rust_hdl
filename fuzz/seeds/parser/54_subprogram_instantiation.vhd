package p is
  function f is new work.gf [bit, bit return bit];
  procedure q is new work.gq [bit] generic map (g => 1);
  function h is new work.gh generic map (t => bit);
end package;
