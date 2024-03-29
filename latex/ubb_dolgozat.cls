%%      ubb_dolgozat.cls
%%
%%      Babes-Bolyai Egyetem stilusfile-ja
%%
%%      (c) Csató Lehel 2010, 2013, 2018
%%      (c) Bodó Zalán 2015 :)
%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ubb_dolgozat}
              [2018/05/02 v1.1
              Babes-Bolyai Egyetem stilus-file.]

\newcommand\@ptsize{}
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
\newif\if@openright
\newif\if@openbib
\@openbibfalse
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\DeclareOption{10pt}{\renewcommand\@ptsize{0}}
\DeclareOption{11pt}{\renewcommand\@ptsize{1}}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{\@openbibtrue}
\ExecuteOptions{a4paper,11pt,oneside,onecolumn,final,openany}
\ProcessOptions
\RequirePackage{pgf}
\input{size1\@ptsize.clo}
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.5}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.5}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
\renewcommand\dblfloatpagefraction{.5}
\if@twoside
    \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
      \def\chaptermark##1{%
        \markboth {\uppercase{\ifnum \c@secnumdepth >\m@ne
            \thechapter.\ \@chapapp:\ \ \fi
            ##1}}{}}%
      \def\sectionmark##1{%
        \markright {{\ifnum \c@secnumdepth >\z@
            \thechapter.\thesection. \ \fi
            ##1}}}}
\else
    \def\ps@headings{%
      \def\@oddfoot{\hfil\textrm{\thepage}\hfil}
      \def\@oddhead{{\scshape\rightmark}\hfil}%
      \let\@mkboth\markboth
      \def\chaptermark##1{%
        \markright {{\ifnum \c@secnumdepth >\m@ne
            \thechapter.\ \@chapapp:\ \ \fi
            ##1}}}}
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }
\if@titlepage
	\newcommand\maketitle{\begin{titlepage}%
    \let\footnotesize\small%
    \let\footnoterule\relax%
    %% ENG
    \vspace*{-20mm}%
	  \thispagestyle{empty}%
    {\fontsize{18}{20}\selectfont\scshape\noindent\@uniEN\par}%
    \vspace{25mm}
    \begin{center}%
  	  \fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont\bfseries
      Diploma Thesis\par
      \vspace*{2mm}
      \fontsize{26}{28}\selectfont\@titleEN \par%
      \vspace*{10mm}
      \pgfimage[width=7.5cm]{logo_ubb}
    \end{center}
    \vfill
    {\fontsize{16}{18}\selectfont\scshape\noindent%
      \begin{minipage}[t]{0.6\linewidth}%
        \baselineskip 2ex \parskip 0.5ex
        \noindent Advisor:\vspace*{0.3cm}
        
        \@tutorEN
      \end{minipage}
      \hfill
      \begin{minipage}[t]{0.4\linewidth}%
        \raggedleft\noindent%
        \baselineskip 2ex \parskip 0.5ex
        Student:\vspace*{0.3cm}
        
        \@author
      \end{minipage}
    }
    \vspace*{0mm}
    \begin{center}%
   	  \fontsize{18}{20}\selectfont\scshape \@submityear %
    \end{center}
    \vspace{-1.6cm}%
    % page
    \newpage
    %% RO
    \vspace*{-20mm}%
	  \thispagestyle{empty}%
    {\fontsize{18}{20}\selectfont\scshape\noindent\@uniRO\par}%
    \vspace{25mm}
    \begin{center}%
  	  \fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont
      \bfseries
      Lucrare de licen\c{t}\u{a}\par
      \vspace*{2mm}
      \fontsize{26}{28}\selectfont\@titleRO \par%
      \vspace*{10mm}
      \pgfimage[width=7.5cm]{logo_ubb}
    \end{center}
    \vfill
    {\fontsize{16}{18}\selectfont\scshape\noindent%
      \begin{minipage}[t]{0.6\linewidth}%
				\baselineskip 2ex \parskip 0.5ex
        \noindent Conduc\u{a}tor \c{s}tiin\c{t}ific:\vspace*{0.3cm}
        
        \@tutorRO
      \end{minipage}
      \hfill
      \begin{minipage}[t]{0.4\linewidth}%
        \raggedleft\noindent%
        \baselineskip 2ex \parskip 0.5ex
        Absolvent:\vspace*{0.3cm}
        
        \@author
      \end{minipage}
    }
    \vspace*{0mm}
    \begin{center}%
   	  \fontsize{18}{20}\selectfont\scshape \@submityear %
    \end{center}
    \vspace{-1.6cm}%
    % page
    \newpage
	  % HU
    \vspace*{-20mm}%
	  \thispagestyle{empty}%
    {\fontsize{18}{20}\selectfont\scshape\noindent\@uniHU\par}%
    \vspace{25mm}
    \begin{center}%
      \fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont
      \bfseries
      Szakdolgozat\par
      \vspace*{2mm}
      \fontsize{26}{28}\selectfont\@titleHU \par%
      \vspace*{10mm}
      \pgfimage[width=7.5cm]{logo_ubb}
    \end{center}
    \vfill
    {\fontsize{16}{18}\selectfont\scshape\noindent%
      \begin{minipage}[t]{0.6\linewidth}%
      	\baselineskip 2ex \parskip 0.5ex
        \noindent T\'emavezet\H{o}:\vspace*{0.3cm}
        
        \@tutorHU
      \end{minipage}
      \hfill
      \begin{minipage}[t]{0.4\linewidth}%
        \raggedleft\noindent%
        \baselineskip 2ex \parskip 0.5ex
        Szerz\H{o}:\vspace*{0.3cm}
        
        \@author
      \end{minipage}
    }
    \vspace*{0mm}
    \begin{center}%
      \fontsize{18}{20}\selectfont\scshape \@submityear%
    \end{center}
    \vspace{-1.6cm}%
	\end{titlepage}%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax%
}
\else
\newcommand\maketitle{\par
  \begingroup
    \renewcommand\thefootnote{\fnsymbol{footnote}}%
    \def\@makefnmark{\hbox to\z@{$\m@th^{\@thefnmark}$\hss}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hbox to1.8em{\hss$\m@th^{\@thefnmark}$}##1}%
    \newpage
    \global\@topnum\z@   % Prevents figures from going at top of page.
    \@maketitle
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\def\@maketitle{%
  \newpage
  \null
  \vskip 1em%
  \begin{center}%
    {\LARGE \@titleHU \par}%
    \vskip 1.5em%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\large \@date}%
  \end{center}%
  \par
  \vskip 1.5em}
\fi
\setcounter{secnumdepth}{2}
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\newcommand\chaptermark[1]{}
\setcounter{secnumdepth}{2}
\renewcommand\thechapter       {\arabic{chapter}}
\renewcommand\thesection       {\thechapter.\arabic{section}}
\renewcommand\thesubsection    {\thesection.\arabic{subsection}}
\renewcommand\thesubsubsection {\thesubsection.\arabic{subsubsection}}
\renewcommand\theparagraph     {\thesubsubsection.\arabic{paragraph}}
\renewcommand\thesubparagraph  {\theparagraph.\arabic{subparagraph}}
%
\newcommand\@chapapp{\chaptername}
\newcommand\part{\cleardoublepage
                 \thispagestyle{plain}%
                 \@tempswafalse
                 \hbox{}\vfil
                 \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}
    {\centering
     \interlinepenalty \@M
     \reset@font
     \ifnum \c@secnumdepth >-2\relax
       \huge\bfseries \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \Huge \bfseries #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \reset@font
     \Huge \bfseries #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@twoside
                \hbox{}%
                \thispagestyle{empty}%
                \newpage
              \fi}
\newcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                         \refstepcounter{chapter}%
                         \typeout{\thechapter.\space\@chapapp}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                    \else
                      \addcontentsline{toc}{chapter}{#1}
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \@makechapterhead{#2}%
                    \@afterheading
                    }
\def\@makechapterhead#1{%
  \vspace*{30\p@}%
  {\parindent \z@ \raggedright \reset@font
    \ifnum \c@secnumdepth >\m@ne
    		{\fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont
         \Large \bfseries \thechapter. \@chapapp{}
        }
         \par
         \vskip 10\p@
       \fi
    {\fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont
      \huge \bfseries #1\par
    }
    %\nobreak
    \vskip 45\p@
  }}
\def\@schapter#1{\@makeschapterhead{#1}%
                 \@afterheading%
                 }
\def\@makeschapterhead#1{%
  \vspace*{30\p@}%
  {\parindent \z@ \raggedright
    \reset@font
    %\huge \bfseries 
    \fontsize{18}{20}\usefont{OT1}{phv}{bc}{n}\selectfont 
    \huge \bfseries
    #1\par
    %\nobreak
    \vskip 30\p@
  }}
\newcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\reset@font\Large\bfseries}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\reset@font\large\bfseries}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\reset@font\normalsize\bfseries}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\reset@font\normalsize\bfseries}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\reset@font\normalsize\bfseries}}
\setlength\leftmargini  {2.5em}
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\setlength\leftmarginv  {1em}
\setlength\leftmarginvi {1em}
\setlength\leftmargin    {\leftmargini}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\arabic{enumi}}
\renewcommand\theenumii{\alph{enumii}}
\renewcommand\theenumiii{\roman{enumiii}}
\renewcommand\theenumiv{\Alph{enumiv}}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{\normalfont\bfseries --}
\newcommand\labelitemii{$\m@th\ast$}
\newcommand\labelitemiii{$\m@th\cdot$}
\newcommand\labelitemiv{{\footnotesize$\m@th\bullet$}}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand\descriptionlabel[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\if@titlepage
\newenvironment{abstractEN}{%
  \newpage
  \titlepage
  \thispagestyle{empty}%
  \vspace*{-20mm}%
  \setstretch{1}
  {\fontsize{18}{20}\selectfont\scshape\noindent\@uniEN\par}%
  \vspace{2mm}
  \begin{center}
    {\bfseries\fontsize{18}{20}\selectfont Diploma Thesis\par}
    \vspace*{1em}
    {\fontsize{26}{28}\selectfont\bfseries \@titleEN \par}%
    \vspace*{1em}
    \bfseries Abstract\\
    \vspace*{.5em}
    \noindent
  \end{center}
  }
  { \par\vspace{5mm}
    \vspace{2em}
    {\noindent\fontsize{16}{18}\selectfont\scshape\@submityear \hfill\@author\par}%
    \vspace*{3em}
    {\noindent\scshape\fontsize{16}{18}\selectfont Advisor:\\
    \@tutorEN}
    \vspace*{-.5cm}
    \endtitlepage
  }
\else
  \newenvironment{abstractEN}{%
      \small%
      \quotation%
      }
      {\endquotation}
\fi
\if@titlepage
\newenvironment{bemutatoHU}{%
  \newpage
  \titlepage
  \setcounter{page}{3}
  \vspace*{-20mm}%
  {\Large\scshape\noindent\@uniHU\par}%
  \vspace{15mm}
  \begin{center}
    {\huge\bfseries \@titleHU \par}%
    \vspace*{4em}
    \bfseries R\"ovid \"osszefoglal\'o
  \end{center}
  }
  { \par\vfill
    \endtitlepage%
    \setcounter{page}{4}
  }
\else
  \newenvironment{bemutatoHU}{%
      \section*{R\"ovid \"osszefoglal\'o}%
      \small
      \quotation
      }
      {\endquotation}
\fi
\newenvironment{verse}
               {\let\\=\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item[]}
               {\endlist}
\newenvironment{quotation}
{\singlespacing
  \list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item[]}
               {\endlist}
\newenvironment{quote}
               {\renewcommand{\baselinestretch}\normalsize
  \list{}{\rightmargin\leftmargin}%
                \item[]}
               {\endlist}
\newenvironment{titlepage}
    {%
      \@restonecolfalse\newpage
      \thispagestyle{plain}%
      \if@compatibility
        \setcounter{page}{0}
      \else
        \setcounter{page}{1}%
      \fi}%
    { \newpage
      \setcounter{page}{2}%
    }
\newcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand\@chapapp{\appendixname}%
  \renewcommand\thechapter{\Alph{chapter}}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset{equation}{chapter}
\renewcommand\theequation{\thechapter.\arabic{equation}}
\newcounter{figure}[chapter]
\renewcommand\thefigure{\thechapter.\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand\thetable{\thechapter.\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\singlespacing \@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1: #2}%
  \ifdim \wd\@tempboxa >\hsize
  \singlespacing
    #1: #2\par
  \else
    \hbox to\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\newcommand{\cal}{\protect\pcal}
\newcommand{\pcal}{\@fontswitch{\relax}{\mathcal}}
\newcommand{\mit}{\protect\pmit}
\newcommand{\pmit}{\@fontswitch{\relax}{\mathnormal}}
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg {2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{2}
\newcommand\tableofcontents{%
    \@restonecolfalse
    \chapter*{\contentsname
        \@mkboth{{\contentsname}}{{\contentsname}}}%
    \@starttoc{toc}%
    }
\newcommand\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}
    \endgroup
  \fi}
\newcommand\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\newcommand\l@section      {\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\l@subsection   {\@dottedtocline{2}{3.8em}{3.2em}}
\newcommand\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\newcommand\l@paragraph    {\@dottedtocline{4}{10em}{5em}}
\newcommand\l@subparagraph {\@dottedtocline{5}{12em}{6em}}
\newcommand\listoffigures{%
    \@restonecolfalse
    \chapter*{\listfigurename
      \@mkboth{\uppercase{\listfigurename}}%
              {\uppercase{\listfigurename}}}%
    \@starttoc{lof}%
    }
\newcommand\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \@restonecolfalse
    \chapter*{\listtablename
      \@mkboth{\uppercase{\listtablename}}{\uppercase{\listtablename}}}%
    \@starttoc{lot}%
    }
\let\l@table\l@figure
\newdimen\bibindent
\bibindent=1.5em
\newenvironment{thebibliography}[1]
     {\chapter*{\bibname
        \@mkboth{\uppercase{\bibname}}{\uppercase{\bibname}}}%
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \if@openbib
              \advance\leftmargin\bibindent
              \itemindent -\bibindent
              \listparindent \itemindent
              \parsep \z@
            \fi
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\arabic{enumiv}}}%
      \if@openbib
        \renewcommand\newblock{\par}
      \else
        \renewcommand\newblock{\hskip .11em \@plus.33em \@minus.07em}%
      \fi
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand\newblock{}
\newenvironment{theindex}
               {\@restonecoltrue
                \columnseprule \z@
                \columnsep 35\p@
                \@makeschapterhead{\indexname}%
                \@mkboth{\uppercase{\indexname}}%
                        {\uppercase{\indexname}}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem  {\par\hangindent 40\p@}
\newcommand\subitem   {\par\hangindent 40\p@ \hspace*{20\p@}}
\newcommand\subsubitem{\par\hangindent 40\p@ \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule width .4\columnwidth
  \kern 2.6\p@}
\@addtoreset{footnote}{chapter}
%% A labjegyzetek formazasa
\long\def\@makefntext#1{%
	\noindent\footnotesize
	\hbox{%
		\vbox{\hbox to 0.02\textwidth{\scriptsize\@thefnmark.}}%
		\vbox{\parbox[t]{0.98\textwidth}{\setstretch{0.5}\noindent{\hspace{-0.1em}}#1}} %
	}%
}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{plain}
\pagenumbering{arabic}      % Arabic page numbers
\if@twoside
\else
  \raggedbottom
\fi

\onecolumn

%%
%% This changes the PAGE outline
%%
\setlength{\hoffset}{-1in}
\setlength{\voffset}{-1in}
%% PAGE SIZES
\topmargin=2.5cm
\oddsidemargin=2.5cm
\evensidemargin=2.5cm
\marginparwidth=0in
\textwidth=6.3in
\hsize=6.3in
\textheight=8.9in
\renewcommand{\baselinestretch}{1.2}
\sloppy
\frenchspacing
%%

%
% MAGYAR ATTRIBUTUMOK
%
\def\uniHU#1{\gdef\@uniHU{#1}}
\gdef\@uniHU{}
\uniHU{Babe\c{s}--Bolyai Tudom\'anyegyetem Kolozsv\'ar\\
  Matematika \'es Informatika Kar\\
  Informatika Szak}
\def\titleHU#1{\gdef\@titleHU{#1}}
\gdef\@titleHU{}
\titleHU{[Dolgozat cim]}
\def\tutorHU#1{\gdef\@tutorHU{#1}}
\gdef\@tutorHU{}
\tutorHU{[Tit. Egy Tanar]}
%
% ROMAN ATTRIBUTUMOK
%
\def\uniRO#1{\gdef\@uniRO{#1}}
\gdef\@uniRO{}
\uniRO{Universitatea Babe\c{s}--Bolyai, Cluj-Napoca\\
  Facultatea de Matematic\u{a} \c{s}i Informatic\u{a}\\
  Specializarea Informatic\u{a}}
\def\titleRO#1{\gdef\@titleRO{#1}}
\gdef\@titleRO{}
\titleRO{[Titlu]}
\def\tutorRO#1{\gdef\@tutorRO{#1}}
\gdef\@tutorRO{}
\tutorRO{[Tit. Un profesor]}
%
% ANGOL ATTRIBUTUMOK
%
\def\uniEN#1{\gdef\@uniEN{#1}}
\gdef\@uniEN{}
\uniEN{Babe\c{s}--Bolyai University of Cluj-Napoca\\
  Faculty of Mathematics and Informatics\\
  Specialization: Computer Science}
\def\titleEN#1{\gdef\@titleEN{#1}}
\gdef\@titleEN{}
\titleEN{[Title]}
\def\tutorEN#1{\gdef\@tutorEN{#1}}
\gdef\@tutorEN{}
\tutorEN{[Teacher, Tit]}
%
% EV
\def\submityear#1{\gdef\@submityear{#1}}
\gdef\@submityear{}
\submityear{2010}

\newenvironment{osszefoglal}{%
      \list{}{\advance\topsep by0.35cm
        \renewcommand{\baselinestretch}{1}\relax\slshape
        \leftmargin=1cm
        \labelwidth=\z@
        \listparindent=\z@
        \vspace*{-1cm}
        \itemindent\listparindent
        \rightmargin\leftmargin}\item[\hskip\labelsep
      \bfseries \"Osszefoglal\'o:]}
    {\endlist}


\pagestyle{headings}

\endinput

%% 
%% End of file `ubb_dolgozat.cls'.
