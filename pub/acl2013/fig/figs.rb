#!/usr/bin/ruby
require 'rfig/FigureSet'
require "#{ENV['HOME']}/workspace/time/aux/figlib.rb"
require 'lib.rb'

################################################################################
# FIGS
################################################################################
def lastFriday
  Parse.new(
    [intersect( ctable('\texttt{\darkred{moveLeft1}}(',friday,')'), dom(13) ),
      [ctable('\texttt{\darkred{moveLeft1}}(',friday,')'),
        ['\texttt{\darkred{moveLeft1$(-)$}}','\darkgreen{last}'],
        [friday,'\darkgreen{friday}'],
      ],
      [dom(13),
        ['\textsf{Nil$_\textsf{the}$}','\darkgreen{the}'],
        [dom(13),'\darkgreen{13$^{\textrm{th}}$}'],
      ],
    ]
  ).constituency
end
def lastFriday13
  Parse.new(
    [ctable('\texttt{\darkred{shiftLeft}}(',intersect(friday,dom(13)),')'),
      [shiftLeft(rangeFig(blue),'1'),'last'],
      [intersect(friday,dom(13)),
        [friday,'friday'],
        [dom(13),
          ['nil','the'],
          [dom(13),'13$^{\textrm{th}}$'],
        ],
      ],
    ]
  ).constituency
end
def lastFridayTypes
  Parse.new(
    ['\texttt{Sequence}',
      ['\texttt{Sequence}',
        ['$f: \texttt{Sequence} \rightarrow \texttt{Sequence}$','\darkgreen{last}'],
        ['\texttt{Sequence}','\darkgreen{Friday}'],
      ],
      ['\texttt{Sequence}',
        ['\texttt{Nil}','\darkgreen{the}'],
        ['\texttt{Sequence}','\darkgreen{13$^{\textrm{th}}$}'],
      ],
  ]).constituency
end
def lastFriday13Types
  Parse.new(
    ['\texttt{Sequence}',
      ['$f: \texttt{Sequence} \rightarrow \texttt{Sequence}$','last'],
      ['\texttt{Sequence}',
        ['\texttt{Sequence}','Friday'],
        ['\texttt{Sequence}',
          ['\texttt{Nil}','the'],
          ['\texttt{Sequence}','13$^{\textrm{th}}$'],
        ]
      ],
    ]
  ).constituency
end
def thisWeek
  Parse.new(
    ['\texttt{Sequence}',
      ['\texttt{Nil}','this'],
      ['\texttt{Sequence}','week']
    ]).constituency
end
def aWeek
  Parse.new(
    ['\texttt{Duration}',
      ['\texttt{Nil}','a'],
      ['\texttt{Duration}','week']
    ]).constituency
end
def may13
  Parse.new(
    ['\texttt{Sequence}',
      ['\texttt{Sequence}','May'],
      ['\texttt{Sequence}','13']
    ]).constituency
end
def may2011
  Parse.new(
    ['\texttt{Range}',
      ['\texttt{Sequence}','May'],
      ['\texttt{Range}','2011']
    ]).constituency
end

def fridayThisWeek
  Parse.new(
    [intersect( friday, everyweek ),
      [friday, '\darkgreen{Friday}'],
      [everyweek,
        ['\textsf{Nil}','\darkgreen{of this}'],
        [everyweek,'\darkgreen{week}'],
      ],
    ]
  ).constituency
end

def fridayDist
  DataTable.new(:cellName => 'Probability',
  :colName => 'Offset',
  :colLabels => [
    rtable(-2,time('10/28/11')).cjustify('c'),
    rtable(-1,time('11/4/11')).cjustify('c'),
    rtable(0,time('11/11/11')).cjustify('c'),
    rtable(1,time('11/18/11')).cjustify('c'),
    rtable(2,time('11/25/11')).cjustify('c')],
  :contents => [[0.1], [0.2], [0.4], [0.15], [0.05]].transpose)
end

def next2daysTypes
  Parse.new(
    ['\texttt{Range}',
      ['$f(\texttt{Duration}):\texttt{Range}$',
        ['\texttt{\cadetblue{catRight}}', '\darkgreen{next}']],
      ['\texttt{Duration}',
        ['\texttt{Number}',
          ['\texttt{\cadetblue{Num$_{n*10^0}$}}', '\darkgreen{2}']],
        ['\texttt{Duration}',
          ['\texttt{\cadetblue{Day}}', '\darkgreen{days}']],
      ],
  ]).constituency
end

def next2days
  Parse.new(
    [ctable('\texttt{\darkred{catRight$(t$}},',time('2D'),'$)$'),
      ['\texttt{\darkred{catRight$(t,-)$}}','\darkgreen{next}'],
      [time('2D'),
        [time('Num(2)'),'\darkgreen{2}'],
        [time('1D'),'\darkgreen{days}'],
      ],
  ]).constituency
end

def sequenceFields(fields)
  def cell(header, contents)
    rtable(
      _(header).bold.color(darkblue),
      contents ? _(contents) : "---",
    nil).center
  end
  table(
    [cell('year', fields[:year]),
     table(
      [cell('mon', fields[:month]), cell('day', fields[:day])],
      [cell('week', fields[:weekOfYear]), cell('weekday', fields[:dayOfWeek])],
     nil).center.margin(u(0.3), u(0.3)),
     cell('hour', fields[:hour]),
     cell('min',  fields[:minute]),
     cell('sec',  fields[:second])],
  nil).center.margin(u(0.3), u(0.3))
end

################################################################################
# FIGURES
################################################################################
initFigureSet(
  :latexHeader => IO.readlines("#{ENV['HOME']}/lib/latex/std-macros.tex") +
                  IO.readlines("#{ENV['HOME']}/workspace/time/pub/acl2013/macros.tex")
)


################################################################################
# SEQUENCE GROUNDING
################################################################################
def sequenceGrounding
  def label(txt)
    _(txt + ":").bold
  end
  ctable(
    rtable(
      ctable(
        label('Sequence'),
        sequenceFields(:month=>'Nov', :day=>'\th{27} -- \th{28}',
                       :hour=>'00', :minute=>'00', :second=>'00'),
      nil).center.cmargin(u(0.4)),
      ctable(
        label('Reference Time'),
        sequenceFields(:year=>'2013', :month=>'Aug', :day=>'\th{06}',
                       :weekOfYear=>'32', :dayOfWeek=>'Tue',
                       :hour=>'03', :minute=>'25', :second=>'00'),
      nil).center.cmargin(u(0.4)),
    nil).center.rmargin(u(0.5)),
    rarrow,
    sequenceFields(:year=>_('2013').bold, :month=>'Nov', :day=>'\th{27} -- \th{28}',
                   :hour=>'00', :minute=>'00', :second=>'00'),
  nil).center.cmargin(u(0.5))
end

printObj(
  :obj => sequenceGrounding.signature(25),
  :outPrefix => 'sequence'
)

################################################################################
# SYSTEM
################################################################################
def feat(featNum, *args)
  ctable("\\textit{#{featNum}}.", '$<$', 
         ctable(*args.map{ |x| 
           [_(x).scale(0.75), ','] }.flatten.reverse.drop(1).reverse
         ).center, 
         '$>$')
end

def features
  def fridayTag
    [Parse.new([friday, '\darkgreen{Friday}']).constituency.scale(0.75),
      rtable(
        feat(1, friday, phrase('Friday')),
      nil)]
  end
  def nilTag
    [Parse.new(['\textsf{Nil}', '\darkgreen{of this}']).constituency.scale(0.75),
      rtable(
        feat(2, '\textsc{Nil}',  phrase('of')),
        feat(3, '\textsc{Nil}',  phrase('this')),
        feat(4, '\textsc{Nil}',  phrase('of this')),
        feat(5, '\textsc{nil\\_bias}'),
      nil)]
  end
  def weekTag
    [Parse.new([everyweek,'\darkgreen{week}']).constituency.scale(0.75),
      rtable(
        feat(6, everyweek,  phrase('week')),
      nil)]
  end
  def nilWeek
    [Parse.new(
      [everyweek, _('\textsf{Nil}'), everyweek]).constituency.scale(0.75),
      rtable(
        feat(7, ctable('\textsc{Nil}\_', phrase('of')),       everyweek),
        feat(8, ctable('\textsc{Nil}\_', phrase('this')),     everyweek),
        feat(9, ctable('\textsc{Nil}\_', phrase('of this')),  everyweek),
        feat(10, '\textsc{Nil}', '\textsc{Sequence}'),
        feat(11, '\textsc{Nil}', everyweek),
      nil)]
  end
  def root
    [Parse.new(
      [intersect( friday, everyweek ),
        friday, everyweek]).constituency.scale(0.75),
      rtable(
        feat(12, '\textsc{Sequence}', '\textsc{Sequence}'),
        feat(13, '\textsc{Intersect}', friday, everyweek),
        feat(14, '\textsc{root\\_valid}'),
      nil)]
  end
  table(
    fridayTag,
    nilTag,
    weekTag,
    nilWeek,
    root,
  nil).rjustify('c').cjustify('cl').cmargin(u(0.25)).rmargin(u(0.25))
end

def sys
  table(
    #(input)
    [
      _('Input (\phrase,$t$)').color(darkblue),
      ctable(
        '(',
        phrase('Friday of this week'),
        ',',
        ground('August 6 2013'),
        ')',
      nil).center,
    nil],
    ['',darrow],
    #(parse)
    [
      rtable(
        _('Latent').color(darkblue),
        _('parse').color(darkblue),
      nil).cjustify('c'),
      fridayThisWeek.scale(0.75),
    nil],
    ['',darrow],
    #(output)
    [
      _('Output \grounded').color(darkblue),
      time('August 9 2013'),
    nil],
  nil).cjustify('c').rjustify('c').rmargin(u(0.70))
end

def sysAndFeatures
  table(
    [sys, features],
    ['(a)', '(b)'],
  nil).cmargin(u(0.1)).rmargin(u(0.5)).center
end

printObj(
  :obj => sysAndFeatures.signature(67),
  :outPrefix => 'system'
)

################################################################################
# Grammar Rule
################################################################################



def singleRule
  Parse.new(
    [ctable(ctable('(', everyweek, time('-1')).center, ',', '\ty{Seq.}', ')').center,
     [ctable('(', time('moveLeft1'), ',', 'Seq.$\rightarrow$Seq.', ')'),
      '\darkgreen{last}'],
     [ctable('(', everyweek, ',', 'Seq.',')').center,
      '\darkgreen{week}'],
    ]
  ).constituency
end

printObj(
  :obj => singleRule.signature(15),
  :outPrefix => 'singleRule'
)

finishFigureSet
