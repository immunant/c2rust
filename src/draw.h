void full_draw(screen_object o, bool in_place)
{
  attr_t old;
  short dummy;
  attr_t new;
  
  attr_get(&old,&dummy,0);

  /*Set the proper color.*/
  new = COLOR_PAIR(o.color);

  /*Special case to get a grey robot*/
  if (o.character == '#') new |= A_DIM;
  if (o.character <= '\32') new |= A_ALTCHARSET;  
  if (o.bold) new |= A_BOLD;

  attrset(new);
  if (in_place)
    {
      printw("%c",o.character);
    }  else {
      mvprintw(o.y,o.x,"%c",o.character); 
      move(o.y,o.x);
    }

  attrset(old);
}

void draw(screen_object o) { full_draw(o,FALSE); }

void draw_in_place(screen_object o) { full_draw(o,TRUE); }

void message(char* message)
{
  move(1,0);
  clrtoeol();
  mvprintw(1,0,"%.*s",COLS,message);
  move(robot.y,robot.x);
  refresh();
}
