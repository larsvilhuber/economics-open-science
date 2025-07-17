\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Revue-economique}[2024/05/26 Classe de document pour la revue économique]
\LoadClass[10pt]{book} % Charge la classe book avec une taille de police de 10pt

\newif\if@ecran % Crée un commutateur pour l'affichage à l'écran
\newif\if@impression % Crée un commutateur pour l'impression
\newif\if@prepacopie % Crée un commutateur pour la préparation de copie

\DeclareOption{ecran}{\@ecrantrue\@impressionfalse} % Active l'affichage à l'écran
\DeclareOption{impression}{\@impressiontrue\@ecranfalse} % Active l'affichage pour impression
\DeclareOption{prepacopie}{\@prepacopietrue} % Active l'affichage pour la préparation de copie
\ExecuteOptions{impression} % Par défaut, l'affichage est configuré pour l'écran
\ProcessOptions\relax % Traite les options du document

% Packages linguistiques
\RequirePackage[english,french]{babel}
\frenchsetup{og=«,fg=»}
\RequirePackage{lettrine}
% Packages supplémentaires
\RequirePackage{changes} % Pour le suivi des modifications et les commentaires éditoriaux
\RequirePackage{graphicx} % Pour la gestion des images
\RequirePackage[absolute,overlay]{textpos}

\setlength{\TPHorizModule}{1mm}  % Définit l'unité de mesure horizontale (ici en millimètres)
\setlength{\TPVertModule}{1mm}   % Définit l'unité de mesure verticale (ici en millimètres)

\RequirePackage{caption} % Pour la gestion des légendes de tableaux et figures
% Redéfinir les légendes des tableaux selon les paramètres linguistiques en cours
\iflanguage{english}{}{%
    \def\frenchtablename{Tableau}
}
\captionsetup{textfont=it, aboveskip=15pt, skip=3pt, justification=centering, labelsep=endash, position=above}
\usepackage{subcaption} % Pour la gestion des figures contenant des sous-figures
\RequirePackage{pdflscape,afterpage} % Pour la gestion des pages en mode paysage
\RequirePackage{xcolor} % Pour la gestion des couleurs
\RequirePackage{geometry} % Pour le format des pages
\if@ecran
    \geometry{
        paperwidth=155mm,
        paperheight=240mm,
        top=22mm,
        bottom=22mm,
        inner=18mm,
        outer=25mm,
        footskip=12mm,
        footnotesep=12mm plus 1pt minus 1pt
    }
\fi
\if@prepacopie
    \geometry{
        paperwidth=210mm,
        paperheight=297mm,
        top=22mm,
        bottom=35mm,
        inner=35mm,
        outer=35mm,
        footskip=12mm,
        footnotesep=12mm plus 1pt minus 1pt
    }
    \renewcommand\normalsize{%
       \@setfontsize\normalsize{12pt}{14.5pt}%
    }
    \AtBeginEnvironment{Article}{%
    \begin{tikzpicture}[remember picture, overlay]
      \node[rotate=90, anchor=north] at (current page.west) {préparation de copie, compilation du \today};
    \end{tikzpicture}}
  \fi
  \if@impression
  \geometry{
      paperwidth=210mm,
      paperheight=240mm,
      layoutwidth=155mm,
      layoutheight=240mm,
      top=22mm,
      bottom=22mm,
      inner=18mm,
      outer=25mm,
      footskip=12mm,
      footnotesep=12mm plus 1pt minus 1pt,
      showcrop,
      layouthoffset=27.5mm,
      layoutvoffset=0mm
  }
\fi
\usepackage{changepage}

% Options pour les notes de bas de page
\usepackage[bottom]{footmisc} % Force les notes en bas de page

\RequirePackage[skip=1pt]{parskip} % Pour la gestion des paragraphes

% Packages supplémentaires pour la gestion avancée des tableaux
\RequirePackage{array,colortbl} % Pour la personnalisation des colonnes de tableau
\RequirePackage{tabularx} % Pour la gestion des tableaux à largeur fixe
\RequirePackage{booktabs} % Pour des lignes horizontales de meilleure qualité
\RequirePackage{tabularray} % tableaux en LaTeX3
\DefTblrTemplate{caption-sep}{default}{\enskip--\enskip} % harmoniser les légendes de tableaux
\DefTblrTemplate{contfoot-text}{default}{\iflanguage{english}{Continued on next page}{(Suite page suivante)}}
\DefTblrTemplate{conthead-text}{default}{\iflanguage{english}{(Continued)}{(Suite)}}
\UseTblrLibrary{amsmath}
\UseTblrLibrary{booktabs}
\UseTblrLibrary{siunitx}

\RequirePackage{threeparttable} % pour englober les tableaux dans un environnement spécifique
\RequirePackage{longtable,calc}
\RequirePackage{multirow} % Pour fusionner des cellules sur plusieurs lignes
% Packages supplémentaires pour la saisie avancée des mathématiques
\RequirePackage{amsmath} % Pour les fonctionnalités mathématiques avancées
\counterwithout{equation}{chapter} % Numérotation des équations sans le numéro de chapitre
\RequirePackage{amssymb} % Pour les symboles mathématiques supplémentaires
\RequirePackage{amsthm} % Pour les théorèmes et les preuves
\addto\captionsenglish{%
    \renewcommand{\proofname}{\textbf{Proof.} \quad}}
\addto\captionsfrench{%
    \renewcommand{\proofname}{\textbf{Preuve.} \quad}}
\renewcommand{\qedsymbol}{$\blacksquare$}
\renewenvironment{proof}[1][\proofname]
 {%
  \par
  \pushQED{\qed}%
  \normalfont \topsep6\p@\@plus6\p@\relax
  \list{}{\leftmargin=\parindent\labelwidth=0pt}
  \item[\hskip\labelsep\itshape #1\@addpunct{.}\hfill]\ignorespaces
 }
 {%
  \popQED\endlist\@endpefalse
 }

\RequirePackage{amsfonts} % Pour les polices mathématiques supplémentaires
\RequirePackage{siunitx}
\RequirePackage{makecell}

\RequirePackage{unicode-math} % Pour utiliser des polices mathématiques Unicode
\RequirePackage{termes-otf} % Pour utiliser la police TeX Gyre Termes
% Définir la police principale et la police mathématique
\setmainfont{TeX Gyre Termes}
\setmathfont{TeX Gyre Termes Math}
% Permettre de modifier la taille du texte relativement à la taille courante
\RequirePackage{relsize} % composer du texte dans une taille relative au texte environnant
\RequirePackage{icomma} % Supprimer l'espace après les virgules dans les maths
\RequirePackage{tcolorbox} % Pour la gestion des boîtes colorées
\tcbuselibrary{skins, breakable} % Charger les bibliothèques nécessaires


\usepackage[protrusion=true,expansion=true]{microtype} % Pour améliorer le gris typographique des lignes
\setsansfont{Arial} % Définit la police sans empattement comme Arial
\RequirePackage{soul}
% Packages supplémentaires pour la gestion des en-têtes de pages
\RequirePackage{fancyhdr} % Pour la personnalisation des en-têtes et pieds de page
\pagestyle{fancy} % Utilise le style fancy pour les en-têtes et pieds de page
\fancyhf{} % Efface les en-têtes et pieds de page existants
\renewcommand{\headrulewidth}{0pt}%
\renewcommand{\footrulewidth}{0pt}%
\fancypagestyle{OuvertureArticle}{%
    \fancyhf{} % Efface les en-têtes et pieds de page existants
    \renewcommand{\headrulewidth}{0pt}%
    \renewcommand{\footrulewidth}{0pt}%
    \fancyfoot[RO,LE]{\thepage} % Numérotation des pages en chiffres arabes
}
\newcommand{\titrecourantauteur}{\relax}
\newcommand{\titrecourantarticle}{\relax}
\fancypagestyle{CourantArticle}{%
    \fancyhf{} % Efface les en-têtes et pieds de page existants
    \renewcommand{\headrulewidth}{0pt}%
    \renewcommand{\footrulewidth}{0pt}%
    \fancyhead[RO]{\def\\{,\space}\footnotesize\itshape\titrecourantauteur}
    \fancyhead[LE]{\footnotesize\itshape\titrecourantarticle}
    \fancyfoot[RO,LE]{\thepage} % Numérotation des pages en chiffres arabes
    \fancyfoot[RE,LO]{\scriptsize\itshape Revue économique -- vol. \volumetitrecourant, \no \numerotitrecourant, \lowercase{\moisanneetitrecourant}, p. \pageref{debutarticle-\thenumeroarticle}-\pageref{finarticle-\thenumeroarticle}}
}
% Les sections sont numérotées jusqu'au niveau 2
\setcounter{secnumdepth}{2}
\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\arabic{section}.\arabic{subsection}}
\renewcommand{\thetable}{\arabic{table}}
\renewcommand{\thefigure}{\arabic{figure}}

% Packages supplémentaires pour la gestion des titres de section
\RequirePackage{titlesec} 
% Définition des titres de section de niveau 1
\titleformat{\section}[block]
{\normalfont\fontsize{11}{13}\selectfont\sffamily}
{\thesection.}
{1em} % était 1em
{\MakeUppercase}

\titlespacing*{\section}{0pt}{8mm}{5mm}

% Définition des titres de section de niveau 2
\titleformat{\subsection}[block]
{\normalfont\fontsize{11}{13}\selectfont\sffamily}{\thesubsection}{1em}{}
\titlespacing*{\subsection}{0pt}{5mm}{5mm}

% Définition des titres de section de niveau 3
\titleformat{\subsubsection}[block]
{\normalfont\fontsize{9}{11}\selectfont\sffamily\itshape}{\thesubsubsection}{1em}{}
\titlespacing*{\subsubsection}{0pt}{5mm}{5mm}

% Packages pour le développement de fonctions avancées
\RequirePackage{etoolbox}
\RequirePackage{xkeyval} % Pour la gestion des clés

% Packages pour la gestion des liens 
\RequirePackage[breaklinks]{hyperref}
\if@ecran
  \hypersetup{colorlinks=true, linkcolor=blue, citecolor=blue, urlcolor=blue}
\else
  \hypersetup{colorlinks=false,pdfborder={0 0 0}}
  \AtBeginDocument{\begin{NoHyper}}
  \AtEndDocument{\end{NoHyper}}
  \RequirePackage[center,a4]{crop}
\fi

% Package pour la gestion des bibliographies
\RequirePackage[babel]{csquotes}

% Gestion de la bibliographie
\RequirePackage[
  style=ext-authoryear-comp, 
  giveninits=true,
  articlein=false,
  backend=biber, 
  language=autobib, 
  doi=false,
  autolang=other, 
  isbn=false, 
  url=false, 
  eprint=false,
  maxcitenames=1, % Maximum 1 auteurs dans les citations
  mincitenames=1, % Utiliser "et al." au-delà de 1 auteurs
  maxbibnames=3,  % Maximum 3 auteurs dans la bibliographie
  minbibnames=1   % Utiliser "et al." au-delà de 1 auteurs dans la bibliographie
]{biblatex}
\DeclareInnerCiteDelims{parencite}{[}{]}
\DeclareInnerCiteDelims{textcite}{[}{]}
\DeclareFieldFormat{biblabeldate}{[#1]}

% Virgule et non deux points entre la ville et l'éditeur
\renewbibmacro*{publisher+location+date}{%
  \printlist{location}%
  \setunit{\addcomma\space}%
  \printlist{publisher}%
  \setunit{\addcomma\space}%
  \usebibmacro{date}%
  \newunit}

% Suppression de la mention "rapp. tech."
\DeclareFieldFormat[report]{type}{}
\DeclareFieldFormat[report]{number}{}
\DeclareBibliographyAlias{techreport}{report}

\DeclareBibliographyDriver{report}{%
  \usebibmacro{bibindex}%
  \usebibmacro{begentry}%
  \usebibmacro{author/editor}%
  \setunit{\labelnamepunct}\newblock
  \usebibmacro{title}%
  \newunit
  \printlist{language}%
  \newunit\newblock
  \usebibmacro{byauthor}%
  \newunit\newblock
  \usebibmacro{byeditor}%
  \newunit\newblock
  \printfield{note}%
  \newunit\newblock
  \usebibmacro{institution+location+date}%
  \newunit\newblock
  \usebibmacro{chapter+pages}%
  \newunit
  \printfield{pagetotal}%
  \newunit\newblock
  \iftoggle{bbx:isbn}
    {\printfield{isbn}}
    {}%
  \newunit\newblock
  \usebibmacro{doi+eprint+url}%
  \newunit\newblock
  \usebibmacro{addendum+pubstate}%
  \setunit{\bibpagerefpunct}\newblock
  \usebibmacro{pageref}%
  \newunit\newblock
  \iftoggle{bbx:related}
    {\usebibmacro{related:init}%
     \usebibmacro{related}}
    {}%
  \usebibmacro{finentry}%
}

% Configuration pour l'ordre des auteurs
\DeclareNameAlias{sortname}{last-first}
\DeclareNameAlias{default}{last-first}

% TODO : On définit la langue des biblios selon la langue des articles
% \DeclareSourcemap{%
%     \maps[datatype=bibtex]{%
%         \map{%
%             \step[fieldset=langid]% , fieldvalue={english}]
%         }%
%     }%
% }%

% \def\blxcsq@resetstyle@langid{%
%   \ifnum\csq@reset=\@ne
%     \iffieldundef{langid}
%       {\let\blxcsq@tempa\languagename}
%       {\edef\blxcsq@tempa{\thefield{langid}}}%
%     \ifx\csq@currentstyle\blxcsq@tempa
%     \else
%       \ifcsundef{csq@qstyle@\blxcsq@tempa}
%         {\csq@warn@style\blxcsq@tempa
%          \csq@setstyle{fallback}}
%         {\csq@setstyle{\blxcsq@tempa}}%
%       \fi
%   \fi}
% \AtEveryBibitem{\let\csq@resetstyle\blxcsq@resetstyle@langid}

% Ajoputer des parenthèses autour des numéros de volumes
\renewbibmacro*{volume+number+eid}{%
  \printfield{volume}%
%  \setunit*{\adddot}% DELETED
  \setunit*{\addnbspace}% NEW (optional); there's also \addnbthinspace
  \printfield{number}%
  \setunit{\addcomma\space}%
  \printfield{eid}}
\DeclareFieldFormat[article]{number}{\mkbibparens{#1}}

% Supprimer "pp." des bibliographies ?
% \DeclareFieldFormat[article, incollection, unpublished]{pages}{#1}
% \renewcommand{\bibpagespunct}{\ifentrytype{article}{\addcolon\addspace}{\addperiod\addspace}}

% Redéfinir le style de "et al." dans la bibliographie
\renewbibmacro*{name:andothers}{% Based on name:andothers from biblatex.def
  \ifboolexpr{
    test {\ifnumequal{\value{listcount}}{\value{liststop}}}
    and
    test \ifmorenames
  }
    {\ifnumgreater{\value{liststop}}{1}
       {\finalandcomma}
       {}%
     \andothersdelim\bibstring[\emph]{andothers}} % \bibstring added to put "et al" in italic
    {}}

% Redéfinir le titre de la bibliographie
\defbibheading{bibliography}[\iflanguage{english}{REFERENCES}{RÉFÉRENCES BIBLIOGRAPHIQUES}]{%
    \vspace{\baselineskip}
    \begin{center}
        \itshape\MakeUppercase{#1}
    \end{center}
}

% Supprimer les petites caps des titres en français
\DefineBibliographyExtras{french}{\restorecommand\mkbibnamefamily}

% Configuration spécifique pour les articles en français 
\DefineBibliographyExtras{french}{% 
\DeclareNameAlias{default}{last-first} 
\renewcommand*{\revsdnamepunct}{} % Supprimer la virgule entre le nom et le prénom 
}

% Virgules après le journal
\DeclareFieldFormat{journaltitle}{\mkbibemph{#1}\isdot}
\renewbibmacro*{journal+issuetitle}{%
  \usebibmacro{journal}%
  \setunit*{\addcomma\space}%
  \iffieldundef{series}
    {}
    {\newunit
     \printfield{series}%
     \setunit{\addspace}}%
  \usebibmacro{volume+number+eid}%
  \setunit{\addspace}%
  \usebibmacro{issue+date}%
  \setunit{\addcolon\space}%
  \usebibmacro{issue}%
  \newunit}

% Virgules après le titre d'article
\renewcommand*{\newunitpunct}{\addcomma\space}

% Package pour les appendices
\RequirePackage{appendix}
\AtBeginEnvironment{appendices}{
    \vspace{2\baselineskip}
    \begin{center}
        \scshape\itshape\iflanguage{english}{\appendixname}{\appendixname}
    \end{center}
    \setcounter{figure}{0}
    \setcounter{table}{0}
    \setcounter{equation}{0}
    % Définition des titres de section de niveau 1 dans les annexes
    \titleformat{\section}[block] 
    {\normalfont\fontsize{12}{14}\selectfont\rmfamily\scshape\centering}{\thesection.}{1em}{}
    \titlespacing*{\section}{0pt}{5mm}{5mm}
    \renewcommand{\thefigure}{A\arabic{figure}}
    \renewcommand{\thetable}{A\arabic{table}}
    % Redéfinir la numérotation des compteurs
    \renewcommand{\thesection}{\Roman{section}}
    \renewcommand{\thesubsection}{\thesection.\arabic{subsection}}
    \renewcommand{\theequation}{A\arabic{equation}}
    \setcounter{secnumdepth}{2}
}
\AtEndEnvironment{appendices}{
    \renewcommand{\thefigure}{\arabic{figure}}
    \renewcommand{\thetable}{\arabic{table}}
    \setcounter{secnumdepth}{2}
}

% Commandes pour la définition des auteurs des articles.
% Chaque auteur est défini à l'aide du package keyval par la clé Auteur={}.
\if@ecran
    \define@key{article}{Auteur}{%
        \def\Auteur{%
            {\raggedleft #1\par\vspace{.5cm}}
            \renewcommand{\titrecourantauteur}{\makeatletter
            \let\@oldthanks\thanks
            \renewcommand{\thanks}[1]{}
            \makeatother #1}%
        }%
    }
    \fi
    \if@prepacopie
    \define@key{article}{Auteur}{%
        \def\Auteur{%
            {\centering #1\par\vspace{.5cm}}
            \renewcommand{\titrecourantauteur}{\makeatletter
            \let\@oldthanks\thanks
            \renewcommand{\thanks}[1]{}
            \makeatother #1}%
        }%
    }
    \fi
    \if@impression
    \define@key{article}{Auteur}{%
        \def\Auteur{%
            {\raggedleft #1\par\vspace{.5cm}}
            \renewcommand{\titrecourantauteur}{\makeatletter
            \let\@oldthanks\thanks
            \renewcommand{\thanks}[1]{}
            \makeatother #1}%
        }%
    }
\fi

% Commande pour redéfinir localement le titre courant des auteurs (cas où auteurs > 3 par exemple)
\newcommand{\titrecourantauteuretal}[1]{%
    \renewcommand{\titrecourantauteur}{#1}
}

\if@ecran
    \define@key{article}{Titre}{%
        \def\Titre{%
            {%
                \normalfont\fontsize{18}{22}\selectfont\raggedleft #1\par\normalsize\vspace{0.5cm}%
            }%
            \noindent\hrulefill% Ligne horizontale après le titre
            \par\vspace{0.5cm}% Espace vertical après la ligne horizontale
            \thispagestyle{OuvertureArticle} % Utilise le style fancy pour les en-têtes et pieds de page
            \pagestyle{CourantArticle} % Utilise le style fancy pour les en-têtes et pieds de page
        }%
        \renewcommand{\titrecourantarticle}{\def\\{}#1}
    }
    \fi
\if@impression
    \define@key{article}{Titre}{%
        \def\Titre{%
            {%
                \normalfont\fontsize{18}{22}\selectfont\raggedleft #1\par\normalsize\vspace{0.5cm}%
            }%
            \noindent\hrulefill% Ligne horizontale après le titre
            \par\vspace{0.5cm}% Espace vertical après la ligne horizontale
            \thispagestyle{OuvertureArticle} % Utilise le style fancy pour les en-têtes et pieds de page
            \pagestyle{CourantArticle} % Utilise le style fancy pour les en-têtes et pieds de page
        }%
        \renewcommand{\titrecourantarticle}{\def\\{}#1}
    }
    \fi
    \if@prepacopie
    \define@key{article}{Titre}{%
        \def\Titre{%
            {%
                \normalfont\sffamily\fontsize{18}{22}\selectfont\centering #1\par\normalsize\vspace{0.5cm}%
            }%
            \par\vspace{0.5cm}% Espace vertical après la ligne horizontale
            \thispagestyle{OuvertureArticle} % Utilise le style fancy pour les en-têtes et pieds de page
            \pagestyle{CourantArticle} % Utilise le style fancy pour les en-têtes et pieds de page
        }%
        \renewcommand{\titrecourantarticle}{\def\\{}#1}
    } 
\fi


\newcounter{numeroarticle}
\newenvironment{Article}[1][]{%
    \refstepcounter{numeroarticle}
    \refstepcounter{chapter}
    \label{debutarticle-\thenumeroarticle}
    \setkeys{article}{#1}%
    \RE@DefineThanks
    \makeArticletitle
    }
{%
    \par
    \label{finarticle-\thenumeroarticle}
}
\newcommand{\soustitre}[1]{\par\smallskip\noindent\large #1\par}

\newcommand{\makeArticletitle}{\@makearticletitle}

\setcounter{tocdepth}{0}

\def\@makearticletitle{%
    \null
    \Titre\par
    \Auteur\par
%    \Affiliation
    \RE@UseThanks
    \def\dotFFN{.} % on peut maintenant remettre le point après les numéros des notes de bas de page
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gestion de \thanks à l'intérieur des rubriques %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\let\thanks\relax % Valeur par défaut (pour le non développement)
\newtoks\RE@hookfootnote
%
% \RE@DefineThanks met en place le mécanisme pour afficher les appels
% de note et sauvegarder les corps de note. Cette macro doit être
% placée juste avant \twocolumn[...]
%
\newcommand*\RE@DefineThanks{%
  \setcounter{footnote}{0}%
  \global\RE@hookfootnote{}%
  \def\thanks##1{%
    \footnotemark
    \global\RE@hookfootnote\expandafter{%
      \the\RE@hookfootnote
      \stepcounter{footnote}%
      \footnotetext{##1}%
    }%
  }%
  \renewcommand\thefootnote{\alph{footnote}}%
}
%
% \RE@UseThanks affiche les notes de bas de page.
%
\newcommand*\RE@UseThanks{%
  \setcounter{footnote}{0}%
%  \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
 \renewcommand{\thefootnote}{\alph{footnote}}
 \def\dotFFN{.}% vide = pas de point après le symbole
  \the\RE@hookfootnote
  % Code suivant réalisé avant le résumé quand il existe.
  % \def\dotFFN{.}%
  % \setcounter{footnote}{0}%
  % \renewcommand\thefootnote{\@arabic\c@footnote}%
}
\newcommand*\RE@UseThanksBis{%
  \setcounter{footnote}{0}%
%  \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
  \renewcommand{\thefootnote}{\alph{footnote}}
  \def\dotFFN{}%
  \the\RE@hookfootnote
  % Code suivant non réalisé car pas de résumé.
  \def\dotFFN{.}%vide = pas de point après le symbole
  \setcounter{footnote}{0}%
  \renewcommand\thefootnote{\@arabic\c@footnote}%
}

% Commandes pour la définition des titres des articles en français puis en anglais
\newcommand{\titrearticleENG}[1]{%
    \par\vspace{6mm}% Espace vertical avant le titre anglais
    {%
        \selectlanguage{english}
        \normalfont\fontsize{9}{11}\selectfont\scshape\raggedright #1\par%
    }%
    \par\vspace{3mm}% Espace vertical après le titre anglais
}

\newcommand{\titrearticleFR}[1]{%
    \par\vspace{6mm}% Espace vertical avant le titre anglais
    {%
        \selectlanguage{french}
        \normalfont\fontsize{9}{11}\selectfont\scshape\raggedright #1\par%
    }%
    \par\vspace{3mm}% Espace vertical après le titre anglais
}

\AtBeginEnvironment{refsection}{%
    \renewcommand\thefootnote{\@arabic\c@footnote}%
    \setcounter{footnote}{0}%
    }

\newenvironment{resume}{%
    \selectlanguage{french}%
    \par\itshape\sffamily\fontsize{8}{10}\selectfont%
    \setlength{\leftskip}{5mm} % Set the left margin to 5mm
    \setlength{\rightskip}{5mm} % Set the right margin to 5mm
    \setlength{\parindent}{0mm} % Supprime l'indentation des paragraphes
}{%
    \par%
}
\newenvironment{resumeENG}{%
    \selectlanguage{english}%
    \par\itshape\sffamily\fontsize{8}{10}\selectfont%
    \setlength{\leftskip}{5mm} % Set the left margin to 5mm
    \setlength{\rightskip}{5mm} % Set the right margin to 5mm
    \setlength{\parindent}{0mm} % Supprime l'indentation des paragraphes
}{%
    \par
}

\newcommand{\motscles}[1]{%
  {    
    \par\vspace*{2mm}
    \setlength{\leftskip}{5mm} % Set the left margin to 5mm
    \setlength{\rightskip}{5mm} % Set the right margin to 5mm
    \setlength{\parindent}{0mm} % Supprime l'indentation des paragraphes
    {\selectlanguage{french}\itshape\sffamily\fontsize{8}{10}\selectfont\noindent Mots clés : #1}%
    \par
  }%
}
\newcommand{\keywords}[1]{%
  {    
    \par\vspace*{2mm}
    \setlength{\leftskip}{5mm} % Set the left margin to 5mm
    \setlength{\rightskip}{5mm} % Set the right margin to 5mm
    \setlength{\parindent}{0mm} % Supprime l'indentation des paragraphes
    {\selectlanguage{english}\itshape\sffamily\fontsize{8}{10}\selectfont\noindent Keywords: #1}%
    \par
  }%
}
\newcommand{\jelcode}[1]{%
    \par\vspace*{2mm}%
    {    
      \setlength{\leftskip}{5mm} % Set the left margin to 5mm
      \setlength{\rightskip}{5mm} % Set the right margin to 5mm
      \setlength{\parindent}{0mm} % Supprime l'indentation des paragraphes
      {\sffamily\fontsize{8}{10}\selectfont\noindent Classification \textit{JEL} : #1.}\par
    }%
}

% Certains auteurs utilisent la commande \sym pour les astérisques dans leurs tableaux.
\newcommand{\sym}[1]{$^{#1}$}

% Construction semi-automatique du sommaire pour le moment
\newenvironment{pagesommaire}{\newpage\thispagestyle{empty}%
\begin{center}
}{\end{center}\cleardoublepage}

\newcommand*{\volumetitrecourant}{\relax}
\newcommand*{\numerotitrecourant}{\relax}
\newcommand*{\moisanneetitrecourant}{\relax}
\newcommand*{\volume}[1]{\noindent VOLUME #1 \gdef\volumetitrecourant{#1}}
\newcommand*{\numero}[1]{\hfill NUMÉRO #1\gdef\numerotitrecourant{#1}\hfill}
\newcommand*{\moisannee}[1]{\gdef\moisanneetitrecourant{#1}\MakeUppercase{#1}\par\noindent\hrulefill\par\vspace*{1cm}{\LARGE SOMMAIRE\par}\vspace{.5cm}}
\newcommand*{\entreesommaire}[3]{#1\par \textit{#2}\par\pageref{#3}\par\vspace{2.5mm}}

% Commandes pour la mise en forme des tableaux de variation statistiques

\newcommand{\erisque}[1]{\multicolumn{1}{l}{#1}}

\newcommand{\smallraisedtext}[1]{%
    \par
    \raisebox{.5ex}{\smaller (#1)}% Texte de taille réduite dans une boîte surélevée
}
\let\varstats\smallraisedtext

\newcommand{\varstatsvar}[1]{%
    \raisebox{1.15ex}{\smaller #1}% Texte de taille réduite dans une boîte surélevée
}
\newcommand{\varstatsnopar}[1]{%
    \raisebox{1.15ex}{\smaller (#1)}% Texte de taille réduite dans une boîte surélevée
}

% Commande pour définir la taille du texte des tableaux
\newcommand{\tabletextsize}{%
    \renewcommand{\arraystretch}{1.25}% Ajuste l'interligne des tableaux
    \fontsize{8}{10}\selectfont% Définit la taille du texte à 8 points avec un interligne de 10 points
}

\AtBeginEnvironment{tabular}{\tabletextsize}
\AtBeginEnvironment{tabularx}{\tabletextsize}
\AtBeginEnvironment{longtable}{\tabletextsize}
\AtBeginEnvironment{tblr}{\tabletextsize}
\AtBeginEnvironment{longtblr}{\tabletextsize}
\SetTblrStyle{caption-text}{font = \normalsize\itshape}
\SetTblrStyle{caption-tag}{font = \normalsize\scshape}
\newcolumntype{D}[1]{>{\raggedleft\arraybackslash}p{#1}}
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}}
\newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}}
\newcommand{\notedetableau}[1]{%
    \par\vspace{\baselineskip}\noindent\begin{minipage}{\linewidth}
        \footnotesize \iflanguage{english}{\textit{Note:} #1}{\textit{Note :} #1}    
    \end{minipage}%
}
\newcommand{\notedefigure}[1]{%
    \par\vspace{\baselineskip}\noindent\begin{minipage}{\linewidth}
        \footnotesize \iflanguage{english}{\textit{Note:} #1}{\textit{Note :} #1}    
    \end{minipage}%
}
\newcommand{\source}[1]{%
    \par\medskip\begin{minipage}{\linewidth}
        \footnotesize \iflanguage{english}{\textit{Source:} #1}{\textit{Source :} #1}    
    \end{minipage}%
}
\newcommand{\sources}[1]{%
    \par\medskip\begin{minipage}{\linewidth}
        \footnotesize \iflanguage{english}{\textit{Sources:} #1}{\textit{Sources :} #1}    
    \end{minipage}%
}
\newcommand{\legende}[1]{%
    \par\medskip\noindent
    \begin{minipage}{\linewidth}
        \footnotesize #1    
    \end{minipage}\par\medskip%
}

% environnement résultat
\newcounter{resultat}[chapter] % Définir le compteur "resultat" et le réinitialiser à chaque nouveau chapitre
\newsavebox{\RE@resultatbox}
\newenvironment{resultat}
  {\refstepcounter{resultat}%
   \par\vspace{\baselineskip}\noindent%
   \begin{lrbox}{\RE@resultatbox}%
   \hspace*{2.5mm}\begin{minipage}{\dimexpr\linewidth-2mm\relax}%
   \textsc{Résultat \theresultat.}\quad}
  {\end{minipage}%
   \end{lrbox}%
   \noindent%
   \makebox[0pt][l]{\rule[-\dp\RE@resultatbox]{1pt}{\dimexpr\ht\RE@resultatbox+\dp\RE@resultatbox\relax}}%
   \usebox{\RE@resultatbox}\par\medskip}

% Environnement Lemme, clone de l'environnement Resultat 
\newcounter{lemme}[chapter] % Définir le compteur "lemme" et le réinitialiser à chaque nouveau chapitre
\newsavebox{\RE@lemmebox}
\newenvironment{lemme}
  {\refstepcounter{lemme}%
    \par\vspace{\baselineskip}\noindent%
   \begin{lrbox}{\RE@lemmebox}%
   \hspace*{2.5mm}\begin{minipage}{\dimexpr\linewidth-2mm\relax}%
   \textbf{Lemme \thelemme.}\quad}
  {\end{minipage}%
   \end{lrbox}%
   \noindent%
   \makebox[0pt][l]{\rule[-\dp\RE@lemmebox]{1pt}{\dimexpr\ht\RE@lemmebox+\dp\RE@lemmebox\relax}}%
   \usebox{\RE@lemmebox}\par\medskip}

\newcounter{proposition}[chapter] % Définir le compteur "proposition" et le réinitialiser à chaque nouveau chapitre

\newenvironment{proposition}
  {%
    \refstepcounter{proposition}%
    \par%
    \begin{tcolorbox}[
      enhanced, % Activer les fonctionnalités avancées
      breakable, % Permettre la rupture de la boîte sur plusieurs pages
      colback=white, % Fond blanc
      colframe=white, % Pas de bordure visible
      boxrule=0pt, % Pas de bordure
      left=2.5mm,
      right=0mm, 
      beforeafter skip=1mm,
      overlay={%
        \draw[line width=1pt] ([yshift=2.5mm]frame.south west) -- ([yshift=-2.5mm]frame.north west); % Filet vertical à gauche
      },
      before upper={\textsc{Proposition \theproposition.}\quad\itshape}, % Mise en forme du titre
    ]
  }
  {%
    \end{tcolorbox}%
    \par%
  }



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aménagements typographiques propres à la Revue économique
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pas de césure sur les mots de moins de 3 lettres
\lefthyphenmin=3
% pas de césure en bas de page
% TODO
% jamais de césure aux mots qui commencent par une lettre capitale
\uchyph=0
\usepackage[all]{nowidow}
\usepackage[hyphenation,parindent]{impnattypo}

% Permettre un texte en pied de page pour les éventuels remerciements. Apparaît sous les affiliations. Se comporte comme une note de bas de page mais sans appel de note -- TODO
\newcommand\extrafootertext[1]{%
    \bgroup
    \renewcommand\thefootnote{\fnsymbol{footnote}}%
    \renewcommand\thempfootnote{\fnsymbol{mpfootnote}}%
    \footnotetext[0]{#1}%
    \egroup
}

\newenvironment{affiliations}[1]{%
    \extrafootertext{#1}}{}

\newcommand{\remerciements}[1]{\extrafootertext{Remerciements : #1}}
\newcommand{\remerciementsENG}[1]{\extrafootertext{\selectlanguage{english}Acknowledgements : #1}}
