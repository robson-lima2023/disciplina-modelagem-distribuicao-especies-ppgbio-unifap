\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ueap-sdm}[2026/06/16 Template Livro SDM]

\LoadClass[11pt,a4paper,openany]{book}

%------------------------------------------------
% PACOTES
%------------------------------------------------
\RequirePackage{iftex}
\RequirePackage[T1]{fontenc}

\ifPDFTeX
\RequirePackage[utf8]{inputenc}
\fi

\RequirePackage{lmodern}
\RequirePackage[english,portuguese]{babel}
\RequirePackage{geometry}
\RequirePackage{graphicx}
\RequirePackage{xcolor}
\RequirePackage{setspace}
\RequirePackage{tikz}
\RequirePackage{titlesec}
\RequirePackage{tocloft}
\RequirePackage{fancyhdr}
\RequirePackage{lastpage}
\RequirePackage{hyperref}
\RequirePackage{booktabs}
\RequirePackage{longtable}
\RequirePackage{array}
\RequirePackage{amsmath,amssymb}
\RequirePackage{enumitem}
\RequirePackage[most]{tcolorbox}
\RequirePackage{listings}
\RequirePackage{caption}

\IfFileExists{microtype.sty}{\RequirePackage{microtype}}{}
\IfFileExists{fontawesome5.sty}{\RequirePackage{fontawesome5}}{}
\IfFileExists{academicons.sty}{\RequirePackage{academicons}}{}

%------------------------------------------------
% ÍCONES FALLBACK
%------------------------------------------------
\providecommand{\aiOrcid}{\textcolor{UEAPGreen}{\textbf{iD}}}
\providecommand{\faEnvelope}{@}
\providecommand{\faGithub}{GitHub}
\providecommand{\faBook}{[Conceito]}
\providecommand{\faExclamationTriangle}{[!]}
\providecommand{\faFlask}{[Exemplo]}
\providecommand{\faLightbulb}{[Dica]}
\providecommand{\faBullseye}{[Objetivos]}
\providecommand{\faPen}{[Exercícios]}
\providecommand{\faCode}{</>}
\providecommand{\faMapMarked}{[Mapa]}
\providecommand{\faGraduationCap}{[Leitura]}

%------------------------------------------------
% CORES
%------------------------------------------------
\definecolor{UEAPGreen}{RGB}{0,102,51}
\definecolor{UEAPDarkGreen}{RGB}{0,70,40}
\definecolor{UEAPGold}{RGB}{212,175,55}
\definecolor{UEAPBlue}{RGB}{0,76,153}
\definecolor{UEAPDarkBlue}{RGB}{0,45,90}
\definecolor{UEAPGray}{RGB}{60,60,60}
\definecolor{UEAPLightGray}{RGB}{245,247,248}
\definecolor{UEAPCodeBg}{RGB}{248,248,248}
\definecolor{UEAPRed}{RGB}{160,40,40}

%------------------------------------------------
% GEOMETRIA
%------------------------------------------------
\geometry{
	left=2.2cm,
	right=2.0cm,
	top=2.2cm,
	bottom=2.0cm,
	headheight=15pt
}

\onehalfspacing
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.55em}
\setlength{\emergencystretch}{3em}
\raggedbottom

%------------------------------------------------
% LINKS
%------------------------------------------------
\hypersetup{
	colorlinks=true,
	linkcolor=UEAPDarkBlue,
	citecolor=UEAPDarkBlue,
	urlcolor=UEAPBlue
}

%------------------------------------------------
% METADADOS
%------------------------------------------------
\newcommand{\instituicao}[1]{\def\@instituicao{#1}}
\newcommand{\curso}[1]{\def\@curso{#1}}
\newcommand{\programa}[1]{\def\@programa{#1}}
\newcommand{\disciplina}[1]{\def\@disciplina{#1}}
\newcommand{\subtitulo}[1]{\def\@subtitulo{#1}}
\newcommand{\autor}[1]{\def\@autor{#1}}
\newcommand{\emailautor}[1]{\def\@emailautor{#1}}
\newcommand{\githubautor}[1]{\def\@githubautor{#1}}
\newcommand{\orcidautor}[1]{\def\@orcidautor{#1}}
\newcommand{\localano}[1]{\def\@localano{#1}}
\newcommand{\versao}[1]{\def\@versao{#1}}
\newcommand{\logoueap}[1]{\def\@logoueap{#1}}
\newcommand{\isbnlivro}[1]{\def\@isbnlivro{#1}}
\newcommand{\doilivro}[1]{\def\@doilivro{#1}}

\instituicao{Universidade do Estado do Amapá}
\curso{}
\programa{}
\disciplina{Modelagem de Distribuição de Espécies em R}
\subtitulo{}
\autor{}
\emailautor{}
\githubautor{}
\orcidautor{}
\localano{Macapá, Amapá -- 2026}
\versao{Versão 1.0}
\logoueap{Brasao_UEAP.png}
\isbnlivro{ISBN a definir}
\doilivro{DOI a definir}

%------------------------------------------------
% CABEÇALHO E RODAPÉ
%------------------------------------------------
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\small\textcolor{UEAPDarkGreen}{Modelagem de Distribuição de Espécies em R}}
\fancyhead[R]{\small\textcolor{UEAPDarkGreen}{\nouppercase{\leftmark}}}
\fancyfoot[L]{\small\textcolor{UEAPGray}{\@autor}}
\fancyfoot[R]{\small\textcolor{UEAPGray}{Página \thepage\ de \pageref{LastPage}}}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0.2pt}

\fancypagestyle{plain}{
	\fancyhf{}
	\fancyfoot[C]{\small\textcolor{UEAPGray}{\thepage}}
	\renewcommand{\headrulewidth}{0pt}
	\renewcommand{\footrulewidth}{0pt}
}

%------------------------------------------------
% TÍTULOS
%------------------------------------------------
\titleformat{\chapter}[display]
{\normalfont\bfseries\color{UEAPDarkGreen}}
{\Large UNIDADE \thechapter}
{0.3cm}
{\fontsize{24}{28}\selectfont}
[\vspace{0.15cm}\titlerule]

\titleformat{\section}
{\Large\bfseries\color{UEAPDarkBlue}}
{\thesection}{0.7em}{}

\titleformat{\subsection}
{\large\bfseries\color{UEAPDarkGreen}}
{\thesubsection}{0.7em}{}

%------------------------------------------------
% SUMÁRIO
%------------------------------------------------
\renewcommand{\contentsname}{Sumário}
\renewcommand{\cfttoctitlefont}{\Huge\bfseries\color{UEAPDarkBlue}}
\renewcommand{\cftaftertoctitle}{
	\par\vspace{0.3cm}
	{\color{UEAPGold}\rule{0.65\textwidth}{1.2pt}}
	\vspace{0.8cm}
}
\renewcommand{\cftchapfont}{\bfseries\color{UEAPDarkBlue}}
\renewcommand{\cftchappagefont}{\bfseries\color{UEAPDarkBlue}}
\renewcommand{\cftchapleader}{\cftdotfill{\cftdotsep}}
\setlength{\cftbeforechapskip}{0.55em}
\renewcommand{\cftsecfont}{\color{UEAPDarkBlue}}
\renewcommand{\cftsecpagefont}{\color{UEAPDarkBlue}}
\setlength{\cftbeforesecskip}{0.15em}
\setlength{\cftsecindent}{1.2em}
\setlength{\cftsecnumwidth}{2.6em}

%================================================
% ELEMENTOS PRÉ-TEXTUAIS DO LIVRO
%================================================

%------------------------------------------------
% FOLHA DE ROSTO
%------------------------------------------------
\newcommand{\folhaderosto}{
	\begin{titlepage}
		\thispagestyle{empty}
		
		\begin{center}
			
			\vspace*{1.0cm}
			
			\IfFileExists{\@logoueap}{
				\includegraphics[width=3.0cm]{\@logoueap}
			}{
				\fbox{\Large UEAP}
			}
			
			\vspace{0.8cm}
			
			{\Large\bfseries\textcolor{UEAPDarkGreen}{\@instituicao}}
			
			\vfill
			
			{\fontsize{28}{32}\selectfont
				\bfseries
				\textcolor{UEAPDarkBlue}{\@title}
			}
			
			\vspace{0.6cm}
			
			{\Large
				\textcolor{UEAPGray}{\@subtitulo}
			}
			
			\vspace{1.2cm}
			
			{\Large\bfseries \@autor}
			
					
			\vfill
			
			\begin{tcolorbox}[
				colback=UEAPLightGray,
				colframe=UEAPDarkGreen,
				boxrule=0.7pt,
				arc=2mm,
				width=0.86\textwidth,
				left=8pt,
				right=8pt,
				top=8pt,
				bottom=8pt
				]
				\small
				\textbf{Sobre esta obra.}
				
				Este livro apresenta os fundamentos teóricos, metodológicos e computacionais da
				Modelagem de Distribuição de Espécies (\textit{Species Distribution Modeling} -- SDM),
				integrando conceitos de nicho ecológico, biogeografia, ecologia espacial,
				mudanças climáticas e conservação da biodiversidade. A obra combina bases
				conceituais sólidas, exemplos ecológicos reais e aplicações práticas em
				ambiente R, oferecendo ao leitor um fluxo completo de trabalho para construção,
				avaliação e interpretação de modelos ecológicos modernos.
			\end{tcolorbox}
			
			\vfill
			
			{\large \@localano}
			
		\end{center}
	\end{titlepage}
	\clearpage
}

% Compatibilidade com documentos antigos
\newcommand{\capaueap}{\folhaderosto}

%------------------------------------------------
% PÁGINA DE COPYRIGHT
%------------------------------------------------
\newcommand{\copyrightpage}{
	\thispagestyle{empty}
	
	\vspace*{\fill}
	
	{\small
		
		\textbf{\@title}
		
		\@subtitulo
		
		\vspace{0.5cm}
		
		© 2026 \@autor
		
		Todos os direitos reservados.
		
		Nenhuma parte desta publicação poderá ser reproduzida, armazenada ou
		transmitida por quaisquer meios sem autorização prévia do autor, exceto para
		fins acadêmicos, educacionais e científicos, mediante citação adequada da
		fonte.
		
		\vspace{0.5cm}
		
		Primeira edição -- 2026
		
		\@instituicao
		
		\@isbnlivro
		
		\@doilivro
		
		\vspace{0.5cm}
		
		Documento preparado em \LaTeX{} com template editorial desenvolvido para
		obra científica sobre Modelagem de Distribuição de Espécies em R.
	}
	
	\vspace*{\fill}
	
	\clearpage
}

%------------------------------------------------
% SOBRE O AUTOR
%------------------------------------------------
\newcommand{\sobreautor}{
	\thispagestyle{empty}
	
	\begin{center}
		{\Huge\bfseries\textcolor{UEAPDarkBlue}{Sobre o Autor}}
		
		\vspace{0.8cm}
		
		{\Large\bfseries \@autor}
		
		\vspace{0.4cm}
		
		{\large
			\textcolor{UEAPBlue}{\faEnvelope}
			\quad
			\href{mailto:\@emailautor}{\textcolor{UEAPBlue}{\@emailautor}}
		}
		
		\vspace{0.18cm}
		
		{\large
			\textcolor{black}{\faGithub}
			\quad
			\href{\@githubautor}{\textcolor{UEAPBlue}{\@githubautor}}
		}
		
		\vspace{0.18cm}
		
		{\large
			\href{https://orcid.org/\@orcidautor}{
				\IfFileExists{orcid.png}
				{\includegraphics[height=0.38cm]{orcid.png}}
				{\textcolor{UEAPGreen}{\textbf{iD}}}
			}
			\quad
			\href{https://orcid.org/\@orcidautor}
			{\textcolor{UEAPBlue}{https://orcid.org/\@orcidautor}}
		}
	\end{center}
	
	\vspace{1.0cm}
	
	\begin{tcolorbox}[
		colback=UEAPLightGray,
		colframe=UEAPDarkGreen,
		boxrule=0.7pt,
		arc=2mm,
		left=10pt,
		right=10pt,
		top=10pt,
		bottom=10pt
		]
		Robson Borges de Lima é Professor Adjunto da Universidade do Estado do
		Amapá (UEAP), Pesquisador Bolsista de Produtividade em Pesquisa (CNPq) e
		Coordenador do Programa Ecológico de Longa Duração -- Monitoramento
		Integrado de Árvores Gigantes da Amazônia (PELD-MIAG). Atua nas áreas de
		ecologia quantitativa, modelagem ecológica, mensuração florestal, mudanças
		climáticas, biodiversidade tropical, sensoriamento remoto e conservação de
		ecossistemas amazônicos.
		
		Sua produção científica concentra-se na integração entre ecologia,
		modelagem estatística, ciência de dados ambientais, conservação da
		biodiversidade e estudos sobre árvores gigantes da Amazônia. Suas linhas
		de pesquisa incluem Modelagem de Distribuição de Espécies, biomassa e
		carbono florestal, dinâmica de florestas tropicais, sensoriamento remoto,
		ecologia espacial e impactos das mudanças climáticas sobre a biodiversidade.
	\end{tcolorbox}
	
	\clearpage
}

%------------------------------------------------
% PÁGINA INSTITUCIONAL
%------------------------------------------------
\newcommand{\paginainstitucional}{
	\thispagestyle{empty}
	
	\begin{center}
		
		\IfFileExists{\@logoueap}{
			\includegraphics[width=2.6cm]{\@logoueap}
		}{
			\fbox{\Large UEAP}
		}
		
		\vspace{0.6cm}
		
		{\Large\bfseries\textcolor{UEAPDarkGreen}{\@instituicao}}
		
		\vspace{1.0cm}
		
		\rule{0.75\textwidth}{0.8pt}\\[0.35cm]
		{\Large\bfseries\textcolor{UEAPDarkBlue}{Informações editoriais da obra}}\\[0.25cm]
		\rule{0.75\textwidth}{0.8pt}
		
		\vspace{1.0cm}
		
		\begin{tabular}{>{\bfseries\color{UEAPDarkGreen}}p{3.8cm}p{9.2cm}}
			Título: & \@title\\[0.15cm]
			Subtítulo: & \@subtitulo\\[0.15cm]
			Autor: & \@autor\\[0.15cm]
			Versão: & \@versao\\[0.15cm]
			ISBN: & \@isbnlivro\\[0.15cm]
			DOI: & \@doilivro\\[0.15cm]
			Local e ano: & \@localano\\
		\end{tabular}
		
		\vfill
		
		\begin{tcolorbox}[
			colback=white,
			colframe=UEAPDarkGreen,
			boxrule=0.5pt,
			arc=1mm,
			width=0.86\textwidth,
			left=8pt,
			right=8pt,
			top=7pt,
			bottom=7pt
			]
			\small
			Esta obra foi desenvolvida com o objetivo de apoiar a formação de estudantes,
			pesquisadores e profissionais das áreas de ecologia, biogeografia,
			conservação da biodiversidade, ciência de dados ambientais e mudanças
			climáticas. O conteúdo integra fundamentos conceituais, procedimentos
			analíticos e aplicações práticas da Modelagem de Distribuição de Espécies,
			constituindo uma referência para atividades de ensino, pesquisa e tomada de
			decisão em conservação.
		\end{tcolorbox}
		
		\vspace{0.6cm}
		
		{\small\textcolor{UEAPGray}{Documento preparado em \LaTeX{} como livro científico em Modelagem de Distribuição de Espécies em R.}}
		
	\end{center}
	
	\clearpage
}

%------------------------------------------------
% PREFÁCIO
%------------------------------------------------
\newcommand{\prefacio}{
	\chapter*{Prefácio}
	\addcontentsline{toc}{chapter}{Prefácio}
	\markboth{Prefácio}{Prefácio}
	
	A Modelagem de Distribuição de Espécies tornou-se uma das abordagens mais
	importantes da ecologia quantitativa contemporânea. Ao integrar registros de
	ocorrência, variáveis ambientais, algoritmos estatísticos, projeções climáticas
	e aplicações em conservação, a SDM permite investigar como as espécies se
	distribuem no espaço e como poderão responder às mudanças ambientais.
	
	Este livro foi desenvolvido com o objetivo de oferecer uma introdução avançada,
	didática e aplicada à Modelagem de Distribuição de Espécies em R. A obra combina
	fundamentos ecológicos, conceitos biogeográficos, procedimentos computacionais,
	interpretação estatística e aplicações práticas utilizando dados reais,
	especialmente no contexto da biodiversidade amazônica.
	
	O material foi organizado de forma progressiva, conduzindo o leitor desde os
	fundamentos do nicho ecológico até tópicos avançados como modelagem ensemble,
	projeções climáticas futuras, paleoclima, extrapolação, incertezas,
	reprodutibilidade científica e aplicações em conservação.
	
	Espera-se que esta obra contribua para a formação de estudantes, pesquisadores
	e profissionais interessados em ecologia espacial, conservação da biodiversidade,
	mudanças climáticas e ciência de dados ambientais.
	
	\vspace{1cm}
	
	\begin{flushright}
		\@autor\\
		\@localano
	\end{flushright}
	
	\clearpage
}

%------------------------------------------------
% CAIXAS DIDÁTICAS
%------------------------------------------------
\newtcolorbox{conceito}[1][]{
	enhanced, breakable,
	colback=UEAPLightGray,
	colframe=UEAPBlue,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faBook\quad \iflanguage{english}{Concept}{Conceito}},
	fonttitle=\bfseries\color{UEAPBlue},
	coltitle=UEAPBlue,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{atencao}[1][]{
	enhanced, breakable,
	colback=red!4,
	colframe=UEAPRed,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faExclamationTriangle\quad \iflanguage{english}{Attention}{Atenção}},
	fonttitle=\bfseries\color{UEAPRed},
	coltitle=UEAPRed,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{dica}[1][]{
	enhanced, breakable,
	colback=yellow!8,
	colframe=UEAPGold,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faLightbulb\quad \iflanguage{english}{Tip}{Dica}},
	fonttitle=\bfseries\color{UEAPGold},
	coltitle=UEAPGold,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{exemplo}[1][]{
	enhanced, breakable,
	colback=green!4,
	colframe=UEAPGreen,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faFlask\quad \iflanguage{english}{Applied example}{Exemplo aplicado}},
	fonttitle=\bfseries\color{UEAPGreen},
	coltitle=UEAPGreen,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{aplicacao}[1][]{
	enhanced, breakable,
	colback=blue!3,
	colframe=UEAPDarkBlue,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faMapMarked\quad \iflanguage{english}{SDM application}{Aplicação em SDM}},
	fonttitle=\bfseries\color{UEAPDarkBlue},
	coltitle=UEAPDarkBlue,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{leitura}[1][]{
	enhanced, breakable,
	colback=purple!3,
	colframe=purple!60!black,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faGraduationCap\quad \iflanguage{english}{Further reading}{Leitura orientada}},
	fonttitle=\bfseries\color{purple!60!black},
	coltitle=purple!60!black,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{equationbox}[1][]{
	enhanced, breakable,
	colback=UEAPLightGray,
	colframe=UEAPDarkGreen,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	#1
}

\newtcolorbox{objetivos}[1][]{
	enhanced, breakable,
	colback=UEAPLightGray,
	colframe=UEAPDarkBlue,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faBullseye\quad \iflanguage{english}{Learning objectives}{Objetivos da unidade}},
	fonttitle=\bfseries\color{UEAPDarkBlue},
	coltitle=UEAPDarkBlue,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{exercicios}[1][]{
	enhanced, breakable,
	colback=white,
	colframe=UEAPDarkGreen,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faPen\quad \iflanguage{english}{Review exercises}{Exercícios de fixação}},
	fonttitle=\bfseries\color{UEAPDarkGreen},
	coltitle=UEAPDarkGreen,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

%------------------------------------------------
% CAIXAS EDITORIAIS SDM
%------------------------------------------------
\newtcolorbox{sdmkeybox}[1][]{
	enhanced, breakable,
	colback=UEAPGreen!5,
	colframe=UEAPGreen,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faBook\quad \iflanguage{english}{Key message}{Mensagem-chave}},
	fonttitle=\bfseries\color{UEAPGreen},
	coltitle=UEAPGreen,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{sdmwarningbox}[1][]{
	enhanced, breakable,
	colback=red!4,
	colframe=UEAPRed,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faExclamationTriangle\quad \iflanguage{english}{Methodological caution}{Atenção metodológica}},
	fonttitle=\bfseries\color{UEAPRed},
	coltitle=UEAPRed,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{sdmpracticebox}[1][]{
	enhanced, breakable,
	colback=blue!3,
	colframe=UEAPDarkBlue,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faLightbulb\quad \iflanguage{english}{Best practice}{Boa prática}},
	fonttitle=\bfseries\color{UEAPDarkBlue},
	coltitle=UEAPDarkBlue,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

\newtcolorbox{decisao}[1][]{
	enhanced, breakable,
	colback=blue!3,
	colframe=UEAPDarkBlue,
	boxrule=0pt,
	leftrule=3pt,
	arc=1mm,
	left=8pt, right=8pt, top=7pt, bottom=7pt,
	title={\faRoute\quad \iflanguage{english}{Methodological decision}{Decisão metodológica}},
	fonttitle=\bfseries\color{UEAPDarkBlue},
	coltitle=UEAPDarkBlue,
	attach boxed title to top left={xshift=6pt,yshift=-2mm},
	boxed title style={colback=white,colframe=white,boxrule=0pt},
	#1
}

%------------------------------------------------
% CÓDIGOS R
%------------------------------------------------
\ifXeTeX
\RequirePackage{fontspec}
% Latin Modern Mono acompanha a distribuição TeX e evita dependência de
% fontes do sistema, tornando a paginação reproduzível entre máquinas.
\newfontfamily\RCodeFont{Latin Modern Mono}
\else
\newcommand{\RCodeFont}{\ttfamily}
\fi

\lstdefinelanguage{Rcustom}{
	language=R,
	morekeywords={
		library,data.frame,as.matrix,melt,ggplot,aes,geom_point,labs,
		theme,theme_bw,theme_classic,decostand,prcomp,summary,biplot,
		scale,center,TRUE,FALSE,filter,mutate,select,rename,write_csv,
		read_csv,clean_names,distinct,glimpse,ggsave,tibble
	},
	sensitive=true,
	morecomment=[l]{\#},
	morestring=[b]",
	morestring=[b]'
}

\lstdefinestyle{ueapRstyle}{
	language=Rcustom,
	basicstyle=\RCodeFont\fontsize{10.5}{13}\selectfont,
	keywordstyle=\bfseries\color{UEAPDarkBlue},
	commentstyle=\color{UEAPGreen},
	stringstyle=\color{UEAPRed},
	identifierstyle=\color{black},
	numbers=none,
	showstringspaces=false,
	showspaces=false,
	showtabs=false,
	tabsize=2,
	breaklines=true,
	breakatwhitespace=false,
	keepspaces=true,
	literate={≥}{{$\geq$}}1,
	columns=fullflexible,
	frame=none,
	backgroundcolor=\color{UEAPCodeBg}
}

\newcommand{\rscriptfile}[1]{%
	\begin{tcolorbox}[
		enhanced,
		breakable,
		colback=UEAPCodeBg,
		colframe=UEAPDarkBlue,
		colbacktitle=UEAPDarkBlue,
		coltitle=white,
		boxrule=0.5pt,
		leftrule=3pt,
		arc=1mm,
		left=7pt,
		right=7pt,
		top=7pt,
		bottom=7pt,
		title={\faCode\quad R Script: \texttt{\detokenize{#1}}},
		fonttitle=\bfseries
		]
		\IfFileExists{#1}{
			\lstinputlisting[style=ueapRstyle]{#1}
		}{
			\textbf{Arquivo de script não encontrado:} \texttt{\detokenize{#1}}
			
			Verifique se o arquivo está na pasta correta em relação ao arquivo \texttt{.tex}.
		}
	\end{tcolorbox}
}

\newcommand{\rscriptref}[1]{%
	\begin{tcolorbox}[
		colback=UEAPCodeBg,
		colframe=UEAPDarkBlue,
		boxrule=0.4pt,
		arc=1mm,
		left=7pt,right=7pt,top=5pt,bottom=5pt
	]
		\faCode\quad\textbf{Script integral:}
		\texttt{\detokenize{#1}}
	\end{tcolorbox}%
}

\newtcblisting{rcode}{
	enhanced,
	breakable,
	listing engine=listings,
	listing only,
	colback=UEAPCodeBg,
	colframe=UEAPDarkBlue,
	colbacktitle=UEAPDarkBlue,
	coltitle=white,
	boxrule=0.5pt,
	leftrule=3pt,
	arc=1mm,
	left=7pt,
	right=7pt,
	top=7pt,
	bottom=7pt,
	title={\faCode\quad R Script},
	fonttitle=\bfseries,
	listing options={style=ueapRstyle}
}

\lstdefinestyle{ueapShellStyle}{
	language=bash,
	basicstyle=\RCodeFont\fontsize{10.5}{13}\selectfont,
	keywordstyle=\bfseries\color{UEAPDarkBlue},
	commentstyle=\color{UEAPGreen},
	stringstyle=\color{UEAPRed},
	numbers=none,
	showstringspaces=false,
	breaklines=true,
	columns=fullflexible,
	frame=none,
	backgroundcolor=\color{UEAPCodeBg}
}

\newtcblisting{shellcode}{
	enhanced,
	breakable,
	listing engine=listings,
	listing only,
	colback=UEAPCodeBg,
	colframe=UEAPDarkBlue,
	colbacktitle=UEAPDarkBlue,
	coltitle=white,
	boxrule=0.5pt,
	leftrule=3pt,
	arc=1mm,
	left=7pt,
	right=7pt,
	top=7pt,
	bottom=7pt,
	title={\faTerminal\quad Terminal},
	fonttitle=\bfseries,
	listing options={style=ueapShellStyle}
}

%------------------------------------------------
% FIGURAS, TABELAS E LISTAS
%------------------------------------------------
\captionsetup{
	font=small,
	labelfont=bf,
	labelsep=period
}

\setlist[itemize]{leftmargin=*,topsep=3pt,itemsep=2pt}
\setlist[enumerate]{leftmargin=*,topsep=3pt,itemsep=2pt}

%------------------------------------------------
% COMANDOS ÚTEIS
%------------------------------------------------
\newcommand{\doilink}[1]{\href{https://doi.org/#1}{\textcolor{UEAPBlue}{https://doi.org/#1}}}
\newcommand{\weblink}[2]{\href{#1}{\textcolor{UEAPBlue}{#2}}}

\endinput
