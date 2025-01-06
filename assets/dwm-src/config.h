/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "monospace:size=10" };
static const char dmenufont[]       = "monospace:size=10";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#005577";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };
static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
  {  NULL,      NULL,       NULL,       0,            False,       -1 },
};


/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[S]",      stacking }, /* first entry is default */ 
  { "[T]",      tile },
  { "[F]",      NULL },    /* no layout function means floating behavior */
};

/* key definitions */
#define MOD Mod4Mask
#define ALT Mod1Mask
#define CTRL ControlMask
#define SHIFT ShiftMask

static const Key keys[] = {
/*  mod/keys                   function        arguments */
  { MOD, XK_Return,            spawn,          {.v = "kitty" } },
  { MOD, XK_e,                 spawn,          {.v = "thunar" } },
  { MOD, XK_space,             spawn,          {.v = "rofi -show drun" } },
  { MOD, XK_b,                 togglebar,      {0} },

  { 0, XF86AudioRaiseVolume,   spawn,          {.v = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+"} },
  { 0, XF86AudioLowerVolume,   spawn,          {.v = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-"} },
  { 0, XF86AudioMute,          spawn,          {.v = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"} },
  { 0, XF86MonBrightnessUp,    spawn,          {.v = "brightnessctl set 5%+"} },
  { 0, XF86MonBrightnessDown,  spawn,          {.v = "brightnessctl set 5%-"} }, 

  { MOD, XK_q,                 killclient,     {0} },
  { CTRL|ALT, XK_Delete,       quit,           {0} },

  { MOD, XK_Up,                focus,          {.i = +1 } },
  { MOD, XK_Down,              focus,          {.i = -1 } },
  { MOD|SHIFT, XK_Up,          tag,            {.i = +1 } },
  { MOD|SHIFT, XK_Down,        tag,            {.i = -1 } },
  { MOD, XK_Tab,               view,           {0} },
  { MOD|SHIFT, XK_Tab,         tag,            {0} },

  { MOD, XK_r,                 setlayout,      {0} },
  { MOD, XK_m,                 setlayout,      {.v = &layouts[0]} },
  { MOD, XK_f,                 setlayout,      {.v = &layouts[1]} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

