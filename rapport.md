# Contexte

L'objectif du stage est de proposer des heuristiques d'inlining pour le compilateur d'OCaml. L'inlining consiste à injecter le corps d'une fonction en lieu et place d'un appel vers celle-ci dans l'objectif d'accélérer l'exécution du code (ou dans certains cas en diminuer sa taille). Néanmoins copier le corps d'une fonction peut faire augmenter la taille du code et conduire à de grosses pertes de performances lorsque certains seuils sont franchis. Vu la difficulté que serait de faire une analyse approfondie du meilleur choix d'inlining en fonction de tel ou tel processeur l'idée est de se concentrer sur des heuristiques qui fonctionneront bien la plupart du temps. Cette optimisation est actuellement effectuée dans le compilateur natif par la série d'optimisations [flambda](https://v2.ocaml.org/manual/flambda.html), qui sera plus tard remplacé par [flambda2](https://github.com/ocaml-flambda/flambda-backend/tree/main/middle_end/flambda2) actuellement en développement. Découvrir et travailler sur un compilateur complexe comme celui d'OCaml n'a pas été jugé envisageable par mes tuteurs de stage, c'est la raison pour laquelle j'évolue sur un langage "jouet" qui n'est rien d'autre qu'une infime partie d'OCaml. La première moitié du stage a donc consisté à implémenter les différentes phases de la compilation (langages intermédiaires et transformations) nécessaires pour mettre en place et tester lors de la seconde moitié du stage toutes les heuristiques d'inlining possibles qui me semblent pertinentes.

## Langage source

Dans le cadre du stage je ne traite que les fonctionnalités d'OCaml nécessaires pour traiter toutes les cas intéressants de l'inlining et suffisantes pour obtenir un langage Turing-complet. En particulier je me concentre uniquement sur le noyau fonctionnel d'OCaml, les objets, références, exceptions ou autre sont ignorés. Ces choix pourront bien évidemment évoluer en fonction des besoins.

### Lambda-calcul

Les trois règles du lambda-calcul que sont les variables, l'application et l'abstraction sont évidemment des fonctionnalités indispensables.

### Opérations élémentaires

La manipulation des entiers et le branchement conditionnel sont incontournables pour réaliser des programmes dignes de ce nom.

### Fermetures (mutuellement) récursives

La prise en compte de la récursivité est fondamentale, premièrement en terme d'expressivité du langage (je vais être amené à réalisé des tests poussés), deuxièmement il est intéressant de prendre en compte la complexité liée à l'impossibilité d'inliner tous les appels récursifs potentiels. La récursivité mutuelle permet d'ajouter une couche de complexité notamment lors de l'analyse.

### Types Somme et filtrage par motifs

La possibilité de construire des types Somme est une des fonctionnalités essentielles d'OCaml et permet de représenter quasiment n'importe quelle structure de données. De plus, de la même manière que le branchement conditionnel, le filtrage par motifs se prête particulièrement bien à l'inlining puisque connaître le motif peut permettre de filtrer de nombreuses branches et donc d'alléger un potentiel inlining.

## Langage utilisé

Il va de soit qu'OCaml est le meilleur langage pour créer un compilateur pour OCaml, la question ne se pose même pas.

# Phases de compilation

Depuis le premier jour de stage j'ai considérablement augmenté le nombre de langages intermédiaires et transformations au fur et à mesure que la complexité des tâches à réaliser augmentait. J'ai fait des choix qui n'étaient pas toujours judicieux et dû faire machine arrière plusieurs fois ce qui m'a amené à me fixer les règles suivantes :

- Toujours, dans la mesure du raisonnable, pouvoir re-concrétiser après abstraction en gardant sous la main les informations nécessaires à la reconstruction (conserver par exemple la liste des alpha-conversions pour le débogage) ;
- Ne pas réaliser plusieurs transformations à la fois, je pense qu'il est préférable d'avoir un langage intermédiaire en trop plutôt que d'avoir une transformation de l'un vers l'autre trop compliquée ;
- Éviter au mieux les constructions du langage qui ne sont pas utilisées lors d'une transformation mais utilisées dans la suivante, cas dans lequel il peut être pertinent de créer un langage intermédiaire.

Évidemment il est possible que ces règles que je m'applique de respecter ne soient valables que dans le cadre d'un projet de petite taille.

## Analyse lexicale

Les jetons de l'analyse lexicale sont générés comme toujours par OCamllex.

## Analyse syntaxique

L'AST est généré par Menhir.

### Identificateurs

A ce stade, le nom des variables et le nom des constructeurs sont des chaînes de caractères. Je ne me pose pas de question à savoir quel jeu de caractères OCaml est censé supporter.

$var \coloneqq string$

$tag \coloneqq string$

### Filtrage par motif

J'ai choisi de ne supporter que l'essentiel pour ce qui est du filtrage par motif. En particulier il n'est pas possible de déconstruire des termes à la volée ni de filtrer plusieurs motifs par la même expression.

$\text{Deconstructor} : tag \times var^{*} \rightarrow mp$

$\text{Joker} : var \rightarrow mp$

### Opérations binaires

$\text{Add} : bop$

$\text{Sub} : bop$

### Expressions

Les expressions constituent la base d'un langage fonctionnel.

$\text{Var} : var \rightarrow expr$

$\text{Fun} : var \times expr \rightarrow expr$

$\text{App} : expr \times expr \rightarrow expr$

$\text{Let} : var \times expr \times expr \rightarrow expr$

$\text{Let-rec} : (var \times expr)^{*} \times expr \rightarrow expr$

$\text{Int} : int \rightarrow expr$

$\text{Binary} : bop \times expr \times expr \rightarrow expr$

$\text{If} : expr \times expr \times expr \rightarrow expr$

$\text{Type} : var \times (var \times var)^{*} \times expr \rightarrow expr$

$\text{Constructor} : tag \times (expr^{*}) \rightarrow expr$

$\text{Match} : expr \times (mp \times expr)^{*} \rightarrow expr$


## Analyse sémantique

Le CST est construit à partir de l'AST en résolvant les noms et les types (pour l'instant limités aux constructeurs).

### Identificateurs

A ce stade, le nom des variables et le nom des constructeurs sont des chaînes de caractères. Je ne me pose pas de question à savoir quel jeu de caractères OCaml est censé supporter.

$var \coloneqq int$

$tag \coloneqq int$

### Opérations binaires

$\text{Add} : bop$

$\text{Sub} : bop$

### Expressions

Si ce n'est le type des identificateurs, il y a peu de changements.

$\text{Var} : var \rightarrow expr$

$\text{Fun} : var \times expr \rightarrow expr$

$\text{App} : expr \times expr \rightarrow expr$

$\text{Let} : var \times expr \times expr \rightarrow expr$

$\text{Let-rec} : (var \times expr)^{*} \times expr \rightarrow expr$

$\text{Int} : int \rightarrow expr$

$\text{Binary} : bop \times expr \times expr \rightarrow expr$

$\text{If} : expr \times expr \times expr \rightarrow expr$

$\text{Constructor} : tag \times (expr^{*}) \rightarrow expr$

$\text{Match} : expr \times (tag \times var^{\*} \times expr)^{\*} \times expr \rightarrow expr$

### Alpha-conversion

Toutes les variables sont alpha-converties et retournées à part (la liste des substitutions ne fait pas partie du CST). Les variables libres dans le programme sont autorisées, également alpha-converties et retournée à part. L'acceptation de variables libres dans le programme permet à mes yeux de faciliter la gestion du non-déterminisme et d'éviter toute ambiguïté lors de l'analyse. En effet les entrées-sorties peuvent être vues comme des variables libres qui ne sont connues qu'au moment de l'exécution du programme, ce qui permet de s'assurer par exemple qu'un affichage sur la sortie ne serait pas optimisé (de la même manière je me pose la question à savoir si la mémoire, dans le cas où je traiterais les effets de bord, peut être modélisée comme une variable libre, ce qui expliciterait le non-déterminisme des effets de bord).

$$
   \begin{align}
      \tag{Int}
      \over \left( \text{Int} ~ i \right) ~ S ~ C \vdash_{\text{cst}} \left( \text{Int} ~ i \right) ~ \emptyset ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Binary}
      \begin{split}
         e_1 ~ A ~ C &\vdash_{\text{cst}} e_1' ~ S_{e_1} ~ V_{e_1} \\
         e_2 ~ \left( A \cup V_{e_1} \right) ~ C &\vdash_{\text{cst}} e_2' ~ S_{e_2} ~ V_{e_2}
      \end{split}
      \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) ~ A ~ C \vdash_{\text{cst}} \left( \text{Binary} ~ \diamond ~ e_1' ~ e_2' \right) ~ \left( S_{e_1} \cup S_{e_2} \right) ~ \left( V_{e_1} \cup V_{e_1} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Fun}
      \begin{split}
         e ~ \left( A \cup \lbrace x = id_x \rbrace \right) ~ C &\vdash_{\text{cst}} e' ~ S_{e} ~ V_{e}
      \end{split}
      \over \left( \text{Fun} ~ x ~ e \right) ~ A ~ C \vdash_{\text{cst}} \left( \text{Fun} ~ id_x ~ e' \right) ~ \left( S_{e} \cup \lbrace id_x = x \rbrace \right) ~ V_{e}
   \end{align} $$

$$
   \begin{align}
      \tag{Var1}
      \begin{split}
         x \in D(A)
      \end{split}
      \over \left( \text{Var} ~ x \right) ~ A ~ C \vdash_{\text{cst}} \left( \text{Var} ~ A\left( x \right) \right) ~ \emptyset ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Var2}
      \begin{split}
         x \notin D(A)
      \end{split}
      \over \left( \text{Var} ~ x \right) ~ A ~ C \vdash_{\text{cst}} \left( \text{Var} ~ id_x \right) ~ \emptyset ~ \lbrace x = id_x \rbrace
   \end{align} $$




### Indexation

Les noms des constructeurs ont reçus un index relatif à leur position dans la déclaration du type qui sera par la suite utilisé dans les filtrages par motifs.

### Typage

Pour l'instant je ne procède à aucune vérification de typage, c'est peu probable que cela change.

## CFG

La conversion CFG transforme le CST en un ensemble (non ordonné) de basic blocs (qui n'est pas à proprement parler un CFG). L'idée est de perdre le moins d'informations possible du programme source tout en ayant sous la main un langage intermédiaire qui permette une analyse simple et puissante.

$$
   \begin{align}
      \tag{Int}
      \over \left( \text{Int} ~ i \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = \text{Int} ~ i; \epsilon \right) ~ \emptyset ~ \emptyset            \end{align} $$

$$
   \begin{align}
      \tag{Var}
      \over \left( \text{Var} ~ v \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = v; \epsilon \right) ~ \lbrace v \rbrace ~ \emptyset
   \end{align} $$

$$
   \begin{align}
      \tag{Let}
      \begin{split}
         e_2 ~ \sigma ~ \Sigma ~ \epsilon &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \quad \Sigma_3 = \Sigma_{e_2} \setminus \lbrace v \rbrace \\
         e_1 ~ v ~ \left( \Sigma \cup \Sigma_3 \right) ~ \epsilon_{e_2} &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{Let} ~ v ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ \left( \Sigma_3 \cup \Sigma_{e_1} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \right)
   \end{align} $$

$$
   \begin{align}
   \tag{Binary}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \left( \Sigma \cup \lbrace \sigma_1 \rbrace \right) ~ (\sigma = \sigma_{e_1} \diamond \sigma_{e_2}; \epsilon) &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_1 ~ \sigma_{e_1} ~ (\Sigma_{e_2} \cup \Sigma) ~ \epsilon_{e_2} &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ \left( \Sigma_{e_1} \cup \Sigma_{e_2} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Fun}
      \begin{split}
         e ~ \sigma_{e} ~ \emptyset ~ \left( \text{Return} ~ \sigma_{e} \right) \vdash_{\text{cfg}} \epsilon_{e} ~ \Sigma_{e} ~ \beta_{e} \quad \Sigma_2 = \Sigma_{e} \setminus \lbrace \sigma_a \rbrace
      \end{split}
      \over \left( \text{Fun} ~ \sigma_a ~ e \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \left( \sigma = \text{Closure} ~ \rho ~ \Sigma_2; \epsilon \right) ~ \Sigma_2 ~ \left( \beta_{e} \sqcup \lbrace \rho = \text{Clos} ~ \Sigma_2 ~ \left( \sigma_a \right) ~ \epsilon_{e} \rbrace \right)
   \end{align} $$

$$
   \begin{align}
      \tag{If}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \Sigma ~ (\text{Ifreturn} ~ \rho_{e_1} ~ \sigma_{e_2} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_2} ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_3 ~ \sigma_{e_3} ~ \Sigma ~ (\text{Ifreturn} ~ \rho_{e_1} ~ \sigma_{e_3} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_3} ~ \Sigma_{e_3} ~ \beta_{e_3} \\
         e_1 ~ \sigma_{e_1} ~ (\Sigma \cup \Sigma_{e_2} \cup \Sigma_{e_3}) ~ (\text{If} ~ \sigma_{e_1} ~ \rho_{e_2} ~ \Sigma_{e_2} ~ \rho_{e_3} ~ \Sigma_{e_3} ~ \Sigma) &\vdash_{\text{cfg}} \epsilon_{e_1} ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split} \\
      \beta_{e_1e_2e_3} = \lbrace \rho_{e_1} = \text{Ifjoin} ~ \sigma ~ \Sigma ~ \epsilon, \rho_{e_2} = \text{Ifbranch} ~ \sigma_{e_2} ~ \Sigma ~ \epsilon_{e_2}, \rho_{e_3} = \text{Ifbranch} ~ \sigma_{e_3} ~ \Sigma ~ \epsilon_{e_3} \rbrace
      \over (\text{If} ~ e_1 ~ e_2 ~ e_3) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_{e_1} ~ (\Sigma_{e_1} \cup \Sigma_{e_2} \cup \Sigma_{e_3}) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \sqcup \beta_{e_3} \sqcup \beta_{e_1e_2e_3} \right)
   \end{align} $$

$$
   \begin{align}
      \tag{App}
      \begin{split}
         e_2 ~ \sigma_{e_2} ~ \left( \Sigma \cup \lbrace \sigma_{e_1} \rbrace \right) ~ \left( \text{Call} ~ \sigma_{e_1} ~ \left( \sigma_{e_2} \right) ~ \rho ~ \Sigma \right) &\vdash_{\text{cfg}} \epsilon_2 ~ \Sigma_{e_2} ~ \beta_{e_2} \\
         e_1 ~ \sigma_{e_1} ~ \left( \Sigma_{e_2} \cup \Sigma \right) ~ \epsilon_2 &\vdash_{\text{cfg}} \epsilon_1 ~ \Sigma_{e_1} ~ \beta_{e_1}
      \end{split}
      \over \left( \text{App} ~ e_1 ~ e_2 \right) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 ~ \left( \Sigma_{e_1} \cup \Sigma_{e_2} \right) ~ \left( \beta_{e_1} \sqcup \beta_{e_2} \sqcup \lbrace \rho = \text{Return} ~ \sigma ~ \Sigma ~ \epsilon \rbrace \right)
   \end{align} $$

$$
   \begin{align}
      \tag{Constructor} f = \begin{cases} f_0 =
      (\text{Constructor} ~ t ~ \alpha) ~ \Sigma ~ \beta \\
      f_n = {f_{n-1} = \epsilon_{n-1} ~ \Sigma_{n-1} ~ \beta_{n-1}
         \quad a_n ~ \alpha_n ~ \alpha \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon_n ~ \Sigma_n ~ \beta_n
         \over \epsilon_n ~ \Sigma_n \cup \Sigma_{n-1} \setminus \lbrace \alpha_n \rbrace ~ \beta_n \cup \beta_{n-1}}
      \end{cases}
      \over (\text{Constructor} ~ t ~ a_n) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} f_n
   \end{align} $$




$$ \begin{align} \begin{cases}
      \epsilon_0 = (\sigma = \text{Constructor} ~ t ~ (\overline{a_1} \dots \overline{a_n}); \epsilon) \\
      \epsilon_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \epsilon }
   \end{cases}
   \begin{cases}
      \Sigma_0 = \lbrace \overline{a_1}, \dots, \overline{a_n} \rbrace \\
      \Sigma_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \Sigma \cup \Sigma_{n-1} \setminus \lbrace \overline{a_n} \rbrace }
   \end{cases}
   \begin{cases}
      \beta_0 = \emptyset \\
      \beta_n =
         { a_n ~ \overline{a_n} ~ \Sigma \cup \Sigma_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \beta \cup \beta_{n-1} }
   \end{cases}
   \over (\text{Constructor} ~ t ~ (a_1 \dots a_n)) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_n ~ \Sigma_n ~ \beta_n \end{align} $$

$$
   \begin{align}
      \tag{Match}
      \begin{split}
         d ~ \overline{d} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{d} ~ \Sigma) \vdash_{\text{cfg}} \epsilon_d ~ \Sigma_d ~ \beta_d \\
         \begin{cases}
            \beta_0 = \emptyset \\
             \beta_n =
            { e_n ~ \overline{e_n} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{e_n} ~ \Sigma) \vdash_{\text{cfg}} \epsilon ~ \Sigma_{e_n} ~ \beta
            \quad \Sigma_{e_n} = \Sigma_{e_n} \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace
            \over \beta \cup \beta_{n-1} \cup \lbrace \langle \rho_{e_n}, \text{Matchbranch} ~ \left( a_n^1 \dots a_n^{m_n} \right) ~ \Sigma_{e_n} ~ \Sigma, \epsilon \rangle \rbrace }
         \end{cases} \\
         { \Sigma_{e_n} =
         e_n ~ \overline{e_n} ~ \Sigma ~ (\text{Matchreturn} ~ \rho_\epsilon ~ \overline{e_n} ~ \Sigma) \vdash_{\text{cfg}} \epsilon ~ \Sigma ~ \beta
         \over \Sigma \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace } \\
         e ~ \sigma_e ~ \left( \bigcup_{i=1}^{n} \Sigma_{e_i} \cup \Sigma_d \cup \Sigma \right) ~ \left( \text{Matchpattern} ~ \sigma_e ~ \left( \langle t_i, \rho_{e_i}, \left( a_i^j \right)^{j=1 \dots m_i}, \Sigma_{e_i} \rangle \right)^{i=1 \dots n} ~ \langle \rho_d, \Sigma_d \rangle ~ \Sigma \right) \vdash_{\text{cfg}} \epsilon_1 ~ \Sigma_1 ~ \beta_1
      \end{split}
      \over (\text{Match} ~ e ~ \left( \langle t_i, \left( a_i^j \right)^{j=1 \dots m_i}, e_i \rangle \right)^{i=1 \dots n} ~ d) ~ \sigma ~ \Sigma ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 ~ (\Sigma_1 \cup \Sigma_2) ~ (\beta_1 \cup \beta_2)\[\rho = \text{Return} ~ \sigma ~ \Sigma ~ \epsilon\]
   \end{align} $$


### Instruction

Une instruction est soit une déclaration soit un branchement. Une déclaration construit une valeur et l'associe à un identifiant unique. Les valeurs ne peuvent être construites qu'à partir de constantes ou identifiants. Un branchement représente le transfert d'un basic block à un autre que ce soit par le biais d'un appel (fermeture), d'un retour de fonction (return) ou d'un saut conditionnel (filtrage par motif).

### Basic blocks

Chaque bloc est construit différemment en fonction de l'expression à partir de laquelle il est construit (fermeture, retour de fonction ou saut conditionnel), est clos (il explicite les arguments dont il a besoin) et contient une suite de déclarations de variables suivies d'une instruction de branchement.

## CFG analysé

L'analyse est l'étape la plus compliquée et probablement la plus importante. Elle s'effectue au niveau du CFG afin d'exploiter la sémantique du langage (et donc de conserver certaines relations) et les informations sur les blocs. Sur conseil de mon tuteur, je réalise une analyse par zone d'allocation. Comme après alpha-conversion chaque nom de variable est unique, on peut identifier les valeurs par un ensemble de noms de variables (ce qui correspond aux endroits potentiels où elles ont été déclarées et initialisées). Pour exploiter l'analyse il m'a été nécessaire dans l'immédiat de cloner le CFG pour intégrer les informations issues de l'analyse. Cela concerne exclusivement les appels indirects transformés en appels directs mais pour lesquels je ne peux pas les transformer en saut (directs) pour rester conforme à la sémantique.

### Abstractions

Actuellement j'utilise deux abstractions pour représenter toutes les valeurs du langage. La première pour les entiers qui est simplement le domaine singleton. La deuxième pour les fermetures (resp. les constructeurs) est un environnement d'identifiant vers contexte, où l'identifiant correspond au pointeur de fonction (resp. au tag), et le contexte correspond aux variables libres (resp. au payload). Étant donné que les pointeurs de fonctions, les tags ainsi que les contextes (ensemble de zones d'allocations) sont des ensembles bornés, l'union de deux abstractions est garantie de converger.

### Preuve de terminaison

Les noms de variables, par extension les zones d'allocations, étant en nombre fini dans le programme, l'ensemble identifiant les valeurs est également fini. De la même manière, un contexte d'appel (ensemble fini de valeurs correspondant aux arguments et pointeur de bloc), est un ensemble fini étant donné que le nombre de blocs dans le programme est également borné. Reste la question épineuse de comment garantir que la pile d'appels ne croît pas infiniment. Afin de tenter d'obtenir une précision maximale, je détecte d'éventuels motifs sur la pile en regardant si 1..N contextes d'appels se répètent et le cas échéant je supprime la répétition. Par exemple la pile d'appels A::B::A::B::C::[] sera remplacée par A::B::C::[]. Après avoir implémenté cette méthode, mes tuteurs m'ont rapidement fait comprendre qu'elle ne pouvait pas garantir la terminaison. Pour la suite du stage je vais certainement devoir durcir la détection de motifs en passant à 0-CFA ou 1-CFA.

## CFG concret

Le CFG concret est construit à partir du CFG en concretisant quasiment tous les traits de langage propres à OCaml. A chaque construction de valeur du langage OCaml est associée une structure de données, la plupart d'entre elles devenant des n-uplets. Tous les types de blocs fusionnent en un seul en fixant la sémantique des sauts (passage de l'environnement comme argument) et chaque type de branchement est transformé en un saut (direct ou indirect) avec la possibilité d'ajouter des contextes d'appel sur la pile (seule l'instruction d'appel ajoute un contexte lors de cette transformation).

### Inlining

L'inlining a lieu sur le CFG concret car les modifications apportées peuvent casser la sémantique d'appel ce qui doit être représenté au niveau de la pile. Inliner un bloc consiste à intégrer son contenu dans le bloc appelant à la place de la dernière instruction (branchement). Chaque argument du bloc inliné est remplacé par la variable qui lui a été assignée lors du branchement par le bloc appelant. Pour l'instant seuls les appels directs peuvent être inlinés. Si lors de l'appel des contextes étaient empilés sur la pile, alors le branchement du bloc inliné en tiendra compte. En particulier, si le branchement du bloc inliné est un retour de fonction celui-ci dépilera la pile et deviendra un saut direct. Dans les autres cas les contextes du bloc appelant sont empilés sur les contextes du bloc appelé, ce qui permet d'avoir des sauts vers l'intérieur d'une fonction.

### Interprétation

C'est le CFG concret que j'interprête pour dans un premier temps m'assurer de la validité de toutes les transformations. Pour la suite du stage je serai amené à extraire de nombreuses informations issues de l'interprétation pour vérifier la pertinance des heuristiques mises en place.

# Heuristiques d'inlining

Les phases de compilation étant prêtes et à mes yeux suffisamment expressives pour permettre toute forme d'inlining, les heuristiques en tant que telles seront traitées lors de la seconde moitié du stage. De nombreux tests complets seront à réaliser pour à la fois vérifier la pertinence des heuristiques implémentées mais également en détecter de nouvelles.

## Inlining partiel

Une heuristique possible sur laquelle je vais me pencher sera l'[inlining partiel](https://developers.redhat.com/blog/2014/10/29/rhel7-gcc-optimizations-partial-inlining), c'est à dire potentiellement inliner seulement les premiers blocs d'une fonction. La manière dont je représente le programme me permet de sauter à l'intérieur d'une fonction, ce qui n'est actuellement pas possible avec flambda2. La question est donc de savoir s'il existe des cas où cette heuristique pourrait être intéressante, que ce soit à la fois en terme d'optimisations de la taille ou du temps d’exécution. Une première idée de situations intéressantes qui me vient à l'esprit est le (très) grand nombre de fonctions en OCaml qui filtrent en début de fonction un de leurs arguments ce qui, à ma connaissance, se représente une fois compilé comme quelques instructions suivies d'un saut. Le filtrage en lui même n'est pas une opération spécialement coûteuse en terme d'espace, on transformerait ici un appel de fonction en un filtrage vers des blocs à "l'intérieur" de celle-ci (avec évidemment les opérations sur la pile qu'il convient de faire comme un appel classique). De plus l'inliner peut permettre de gagner en informations sur le motif, ce qui peut rendre possible de transformer le filtrage en appel direct, les gains seraient considérables.

Un exemple qui me pousse à croire qu'une telle heuristique peut être prometteuse est fourni sur le site de [flambda](https://v2.ocaml.org/manual/flambda.html#ss:flambda-inlining-overview) :

```ocaml
let f b x =
  if b then x else ... big expression ...

let g x = f true x
```

En appliquant les transformations pour faire apparaître les blocs, les branchements et les manipulations de la pile on obtient ce pseudo-code sous forme de basic blocks (sans distinguer les appels directs des appels indirects) :

```ocaml
let rec f b x stack =
  if b then f0 x stack else f1 stack
and f0 x stack =
  f2 x stack
and f1 stack =
  f2 (... big expression ...) stack
and f2 x (p::stack) =
  p x stack
and g x stack =
  f true x (g0::stack)
and g0 x (p::stack) =
  p x stack
```

Dans le cas présent la fonction `f` peut être vue comme un bloc d'une seule instruction. L'idée est donc d'inliner systématiquement ce bloc (pas la fonction complète, d'où la notion d'inlining partiel), et on obtient le code suivant où l'appel vers `f` est inliné dans `g` :

```ocaml
let rec f b x stack =
  if b then f0 x stack else f1 stack
and f0 x stack =
  f2 x stack
and f1 stack =
  f2 (... big expression ...) stack
and f2 x (p::stack) =
  p x stack
and g x stack =
  if true then f0 x (g0::stack) else f1 (g0::stack)
and g0 x (p::stack) =
  p x stack
```

Cet inlining n'est possible que si toutes les branches du `if` empilent les mêmes contextes sur la pile, ce qui est normalement toujours le cas quand le code est bien construit. Avec cet inlining, indépendamment de la valeur de `b`, on économise un saut et on se donne davantage d'informations sur le contexte dans le bloc inliné qui pourraient être utiles à d'éventuelles simplifications. Après simplifications, en supposant que le premier bloc de `f` est systématiquement inliné pour chaque appel et en inlinant les appels terminaux, on obtient le code suivant :

```ocaml
let rec f0 x (p::stack) =
  p x stack
and f1 (p::stack) =
  p (... big expression ...) stack
and g x (p::stack) =
  p x stack
```

Évidemment au stade actuel cette heuristique ne reste qu'une hypothèse et il faudra vérifier à la fois sa faisabilité et son efficacité. Il est par ailleurs fort probable qu'elle ne soit pas possible à mettre en place dans l'évaluateur de bytecode.
