Download

Source

PDF
Actions
   Copy Project
   Word Count
Sync
   Dropbox
   Git
   GitHub
Settings
Compiler

pdfLaTeX
TeX Live version

2022
Main document

main.tex
Spell check

English
Dictionary
Auto-complete

On
Auto-close Brackets

On
Code check

On
Editor theme

overleaf
Overall theme

Default
Keybindings

None
Font Size

12px
Font Family

Lucida / Source Code Pro
Line Height

Normal
PDF Viewer

Overleaf
Help
   Show Hotkeys
   Documentation
   Contact Us
Gestion_de_datos

File outline
Overleaf has upgraded the source editor. You can still use the old editor by selecting "Source (legacy)".

Click to learn more and give feedback
Editor mode.


 
Selection deleted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Masters/Doctoral Thesis ZHAW
% Class File
% Version 1.7 (24/8/20)
% 
% ZHAW class version downloaded from:
% https://github.com/matteodelucchi/ZHAW_thesis-template
% 
% University specific changes released in Version 1.7 were made by:
% Matteo Delucchi
%
% Original class was downloaded from:
% http://www.LaTeXTemplates.com
% 
% Authors:
% Vel (vel@latextemplates.com)
% Johannes Böttcher
%
% Notes:
% 1) This class file defines the structure and layout of the template file (main.tex).
% 2) It has been written in such a way that under most circumstances you should not need
% to edit it; updating it to a newer version will be harder. If you do make changes, please change the name of
% the file and add comments to make your changes more visible.
%
% Class license:
% LPPL v1.3c (http://www.latex-project.org/lppl)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%   CLASS DEFINITION AND PARAMETERS
%----------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}[1996/12/01]
\newcommand{\classname}{MastersDoctoralThesis}
\ProvidesClass{\classname}[2020/08/24 v1.7 LaTeXTemplates.com]
\providecommand{\baseclass}{book}
\RequirePackage{etoolbox}
\RequirePackage{xparse}
\newbool{nolistspace}
\newbool{chapteroneline}
\newbool{listtoc}
\newbool{toctoc}
\newbool{parskip}
\newbool{hyperrefsupport}
\booltrue{hyperrefsupport}
Capítulo 2. Metodología 5
Se obtuvo un archivo CSV con toda la información de estudiantes, este archivo fue
subido mediante DataGrip a una BD MySQL, previamente creada como un servi-
cio RDS de AWS, con estos datos cargados se realizaron procesos mediante R para
obtener información que posteriormente fueron registrados en otra base de datos y
mostrados mediante una aplicación Shiny.
En la imagen 2.2 se puede ver el flujo del proceso en este trabajo de análisis.
IMAGEN 2.2: Herramientas usadas
2.3 Trabajo con los datos
Para el trabajo con R y MySQL se hizo uso de la bilbioteca RMySQL y DBI, el código
para la conexión se encuentra en la sección 4.1.
2.4 Análisis realizados
2.4.1 Niveles educativos
Para el caso del nivel educativo, es importante conocer la cantidad de alumnos
por año en cada nivel, de esta forma identificamos si existe algún incremento o al-
guna disminución, para lo que se deberá realizar diversas acciones contrarrestando
cualquier inconveniente de materiales y de inmobiliarios.
En el apéndice 4.2 está el código fuente de esto.
2.4.2 Evolución mensual de matrículas
• Se hizo una consulta a la Base de Datos donde se encuentra la tabla estudiante.
• Debido que sólo había una columna con la fecha de la matrícula en formato
YYYY-MM-DD, se obtuvo sólo los valores de año y mes para que se puedan
agrupar por este campo.
• Luego se agrupó esta información por el nuevo campo que contiene el año y el
mes.
Capítulo 2. Metodología 6
• Se categorizó el campo mes.
• Se agregó un campo de año para que pueda ser filtrado posteriormente.
El código para obtener la información para este análisis se encuentra en el anexo 4.3.
El resultado de este análisis se encuentra en la sección 3.2.
2.4.3 Discapacidad
Para el procesamiento de la data se realizó limpieza de datos vacíos, posterior a eso
se agrupó y realizó el conteo de estudiantes que tengan alguna discapacidad.
Con estos datos se puede posteriormente filtrar por año para que pueda ser visual-
izada dinámicamente en shiny. En el apéndice 4.4 está el código fuente de esto.
2.4.4 Nacionalidades
Se desea mostrar la cantidad de estudiantes de nacionalidad diferente a la peruana.
Para el procesamiento de los datos no se toma en cuenta a los estudiantes con na-
cionalidad peruana y datos en blanco.
En el apéndice 4.5 está el código fuente de esto.
2.4.5 Género
• Se analizo la distribución del género de los estudiantes por año, para esto nece-
sitamos todos los DNIs válidos para proceder con el conteo, por lo cual se re-
alizó una limpieza a la data.
– Limpieza a los strings de genero ya que se encontraban con saltos de línea:
IMAGEN 2.3
– Para ello solo extraemos una parte del string (6 primeros caracteres) y está
solucionado:
• Se lee la información con un parámetro que quita los registros en blanco, para
evitar DNIs que no sean válidos.
El código se encuentra en el apéndice 4.6
2.4.6 DNI Validados
• Se agrupa el dataframa de estudiantes por el campo de validación de DNI.
Capítulo 2. Metodología 7
IMAGEN 2.4
• Se crea un nuevo DF que contiene el conteo del campo que representa la vali-
dación de un DNI.
• Se obtiene un porcentaje de los calores del paso anterior.
El código se encuentra en el apéndice 4.7
2.5 Uso de shiny
Se usó Shiny [2] para mostrar los análisis de manera interactiva, ya que se tiene
información de 3 años (2020, 2021 y 2022).
Los componentes utilizados en el proyecto fueron tablas, gráfico de barras, gráfico
circular y gráfico de líneas [3]. El código para realizar esta funcionalidad se encuen-
tra en el anexo 4.8.
Para la publicación se hace uso del servidor que brinda RStudio [4].
2.6 Fuentes
Las fuentes del proyecto se encuentran en el repositorio Git: https://github.
com/Maestria-Data-Science-UPC-G1/gestiondatos.git
