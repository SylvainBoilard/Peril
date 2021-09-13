type t = {
    name: string;
    color: Color.hsla;
    color_suite: Color.suite;
    mutable defeated: bool;
    mutable reinforcements: int;
    mutable cards: int list;
  }

let make name color =
  { name; color; color_suite = Color.make_suite color;
    defeated = false; reinforcements = 0; cards = [] }
