package p is
  type enum_t is (a, b, 'x', 'y');
  type int_t is range 0 to 255;
  type real_t is range -1.0 to 1.0;
  type time_t is range 0 to 1e9
    units
      fs;
      ps = 1000 fs;
      ns = 1000 ps;
    end units time_t;
end package;
