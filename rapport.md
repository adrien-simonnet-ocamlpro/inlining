---
title:    Heuristique d'inlining complexe
author:   Adrien Simonnet
date:     Avril - Septembre 2023
header-includes:
  - \usepackage{mathtools}
  - \usepackage[ruled,vlined,linesnumbered]{algorithm2e}
geometry: margin=2.5cm
fontsize: 12pt
toc: true
toc-title: Table des matières
abstract: |
  Ceci constitue mon rapport de stage de fin de cursus [Science et Technologie du Logiciel (STL)](https://sciences.sorbonne-universite.fr/formation-sciences/masters/master-informatique/parcours-stl) à [Sorbonne Université](https://sorbonne-universite.fr) réalisé chez [OCamlPro](https://ocamlpro.com/) au sein de l'équipe [flambda](https://v2.ocaml.org/manual/flambda.html). Mon stage a consisté à proposer des heuristiques d'inlining pour le compilateur du langage OCaml, spécialité de l'entreprise. Découvrir et travailler sur un compilateur complexe comme celui-ci n'a pas été jugé envisageable par mes tuteurs de stage, Vincent Laviron et Pierre Chambart, c'est la raison pour laquelle j'ai évolué sur un langage "jouet". L'évolution qu'a prise le stage et le fait qu'il ait commencé tard a fait que j'ai davantage travaillé sur les différentes représentations intermédiaires et analyses nécessaires à l'inlining que sur les heuristiques en elles-mêmes. Mon stage n'étant pas terminé à l'issu de ce rapport, je compte bien l'achever en corrigeant ce dernier point. 
include-before: \newpage
---

\newpage

# Introduction

L'inlining consiste à injecter le corps d'une fonction en lieu et place d'un appel vers celle-ci dans l'objectif d'accélérer l'exécution du code (ou dans certains cas en diminuer sa taille). Néanmoins copier le corps d'une fonction peut faire augmenter la taille du code et conduire à de grosses pertes de performances lorsque certains seuils sont franchis. Vu la difficulté que serait de faire une analyse approfondie du meilleur choix d'inlining en fonction de tel ou tel processeur l'idée a été de se concentrer sur des heuristiques qui fonctionneront bien la plupart du temps. Cette optimisation est actuellement effectuée dans le compilateur natif par la série d'optimisations [flambda](https://v2.ocaml.org/manual/flambda.html), qui sera plus tard remplacé par [flambda2](https://github.com/ocaml-flambda/flambda-backend/tree/main/middle_end/flambda2) actuellement en développement. Le langage "jouet" sur lequel j'ai travaillé n'est rien d'autre qu'une petite partie du noyeau fonctionnel d'OCaml.

## Langage source

Dans le cadre du stage je n'ai traité que les fonctionnalités d'OCaml nécessaires pour aborder la plupart des cas intéressants de l'inlining et suffisantes pour obtenir un langage Turing-complet. En particulier comme je ne me suis concentré que sur le noyau fonctionnel d'OCaml, les objets, références, exceptions ou autres fonctionnalités du langage ont été ignorés.

### Lambda-calcul

Les [trois règles du lambda-calcul](https://fr.wikipedia.org/wiki/Lambda-calcul#Syntaxe) que sont les variables, l'application et l'abstraction sont évidemment des fonctionnalités indispensables.

```ocaml
let id = fun x -> x in
let x = id y in x
```

### Opérations élémentaires

La manipulation des entiers et le branchement conditionnel sont incontournables pour réaliser des programmes dignes de ce nom.

```ocaml
if c then x + 1 else x - 1
```

### Fermetures (mutuellement) récursives

La prise en compte de la récursivité est fondamentale, premièrement en terme d'expressivité du langage (pour réaliser des tests poussés), deuxièmement il est intéressant de prendre en compte la complexité liée à l'impossibilité d'inliner tous les appels récursifs potentiels. La récursivité mutuelle permet d'ajouter une couche de complexité notamment lors de l'analyse.

```ocaml
let rec f = fun x -> g x
and g = fun y -> f y in f 0
```

### Types Somme et filtrage par motifs

La possibilité de construire des types Somme est une des fonctionnalités essentielles d'OCaml et permet de représenter quasiment n'importe quelle structure de données. De plus, de la même manière que le branchement conditionnel, le filtrage par motifs se prête particulièrement bien à l'inlining puisque connaître le motif peut permettre de filtrer de nombreuses branches et donc d'alléger un potentiel inlining.

```ocaml
type int_list =
| Nil
| Cons of int * int_list

let rec map = fun f -> fun l ->
   match l with
   | Nil -> Nil
   | Cons (x, ls) -> Cons (f x, map f ls)
in map (fun x -> x + 10) (Cons (1, Cons (2, Nil)))
```

## Langage utilisé

J'ai réalisé le compilateur en OCaml, en cohérence avec le langage source et l'expertise d'OCamlPro.

\newpage

# Analyse lexicale

L'analyse lexicale est la première étape de la compilation et convertit le programme source vu comme une chaîne de caractères en une liste de jetons.

## Lexique

Le lexique source est un sous-ensemble de celui d'[OCaml](https://v2.ocaml.org/releases/5.0/manual/lex.html) pour supporter les fonctionnalités qui m'intéressent. Je n'ai pas jugé pertinent d'indiquer le nom des jetons.

- `(* *)` pour les commentaires
- `( )` pour le parenthésage
- `+ -` pour les opérations sur les entiers
- `if then else` pour le branchement conditionnel
- `fun ->` pour la création de fermeture
- `let rec = and in` pour les déclarations (mutuellement récursives)
- `type of | *` pour la déclaration d'un type somme
- `,` pour les constructeurs
- `match with | _` pour le filtrage par motif
- `['0'-'9']+` pour la déclaration d'un entier (positif)
- `['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*` pour désigner un constructeur
- `['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*` pour désigner une variable
- `[' ' '\t' '\n' '\r']` pour l'indentation et les sauts de ligne/retours chariot

## Analyseur lexical

Les jetons de l'analyse lexicale sont générés par [OCamllex](https://v2.ocaml.org/manual/lexyacc.html).

\newpage

# Analyse syntaxique

L'analyse syntaxique est la seconde étape de la compilation et va convertir les jetons en un arbre de syntaxe abstraite.

## Arbre de syntaxe abstraite (AST)

La grammaire est la même que celle d'[OCaml](https://v2.ocaml.org/releases/5.0/manual/language.html) pour l'ensemble des jetons supportés.

### Identificateurs

Le nom des variables et le nom des constructeurs sont des chaînes de caractères. Je ne me pose pas de question à savoir quel est le jeu de caractères d'OCaml.

$\mathbb{S} \coloneqq string$ (chaînes de caractères)

$\mathbb{V} \coloneqq \mathbb{S}$ (variables)

$\mathbb{T} \coloneqq \mathbb{S}$ (tags)

### Filtrage par motif

J'ai choisi de ne supporter que l'essentiel pour ce qui est du filtrage par motif. En particulier il n'est pas possible de déconstruire des termes à la volée ni de filtrer plusieurs motifs par la même expression.

$\text{Deconstructor} : \mathbb{T} \times \mathbb{V}^{*} \mapsto \mathbb{M}$ pour filtrer un tag.

$\text{Joker} : \mathbb{V} \mapsto \mathbb{M}$ pour filtrer tous les cas restants.

### Opérateurs binaires

Les opérateurs binaires se limitent pour l'instant aux opérations sur les entiers.

$\text{Add} : \mathbb{B}$ correspond à l'addition.

$\text{Sub} : \mathbb{B}$ correspond à la soustraction.

### Expressions

Les expressions constituent la base d'un langage fonctionnel. Je ne réalise aucune vérification de typage, c'est pour cela que je traite les types comme de simples chaînes de caractères.

$\text{Var} : \mathbb{V} \mapsto \mathbb{E}$ désigne une variable.

$\text{Fun} : \mathbb{V} \times \mathbb{E} \mapsto \mathbb{E}$ fabrique une fermeture.

$\text{App} : \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ applique une fermeture à son argument.

$\text{Let} : \mathbb{V} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ associe le résultat d'une expression à une variable.

$\text{LetRec} : (\mathbb{V} \times \mathbb{E})^{*} \times \mathbb{E} \mapsto \mathbb{E}$ définit des fermetures récursives (uniquement des fermetures).

$\text{Int} : \mathbb{Z} \mapsto \mathbb{E}$ génère un entier.

$\text{Binary} : \mathbb{B} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ représente une opération binaire.

$\text{If} : \mathbb{E} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ est le branchement conditionnel.

$\text{Type} : \mathbb{S} \times (\mathbb{T} \times \mathbb{S})^{*} \times \mathbb{E} \mapsto \mathbb{E}$ fabrique un type (ses constructeurs).

$\text{Constructor} : \mathbb{T} \times \mathbb{E}^{*} \mapsto \mathbb{E}$ fabrique une union taggée à partir d'un constructeur.

$\text{Match} : \mathbb{E} \times (\mathbb{M} \times \mathbb{E})^{*} \mapsto \mathbb{E}$ filtre une union taggée.

## Analyseur syntaxique

L'AST est généré par [Menhir](https://ocaml.org/p/menhir/) à l'aide des jetons générés par l'analyse lexicale.

\newpage

# Analyse sémantique

L'analyse sémantique est la troisième étape de la compilation.

Je ne procède à aucune vérification de typage, cela n'étant pas le sujet principal de mon stage.

## Arbre de syntaxe abstraite' (AST')

L'AST' est construit à partir de l'AST en résolvant les noms et les types (pour l'instant limités aux constructeurs).

### Identificateurs

Le nom des variables et le nom des constructeurs sont désormais représentés par des entiers naturels uniques.

$\mathbb{V} \coloneqq \mathbb{N}$ (variables)

$\mathbb{T} \coloneqq \mathbb{N}$ (tags)

### Opérateurs binaires

Les opérateurs binaires se limitent toujours aux opérations sur les entiers.

$\text{Add} : \mathbb{B}$ correspond à l'addition.

$\text{Sub} : \mathbb{B}$ correspond à la soustraction.

### Expressions

Pour toutes les expressions, à l'exception du filtrage par motif, seul le type des identificateurs change. Les noms des constructeurs ont reçus un index relatif à leur position dans la déclaration du type qui sera par la suite utilisé dans les filtrages par motifs.

$\text{Var} : \mathbb{V} \mapsto \mathbb{E}$ désigne une variable.

$\text{Fun} : \mathbb{V} \times \mathbb{E} \mapsto \mathbb{E}$ fabrique une fermeture.

$\text{App} : \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ applique une fermeture à son argument.

$\text{Let} : \mathbb{V} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ associe le résultat d'une expression à une variable.

$\text{LetRec} : (\mathbb{V} \times \mathbb{E})^{*} \times \mathbb{E} \mapsto \mathbb{E}$ définit des fermetures récursives (uniquement des fermetures).

$\text{Int} : \mathbb{Z} \mapsto \mathbb{E}$ génère un entier.

$\text{Binary} : \mathbb{B} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ représente une opération binaire.

$\text{If} : \mathbb{E} \times \mathbb{E} \times \mathbb{E} \mapsto \mathbb{E}$ est le branchement conditionnel.

$\text{Constructor} : \mathbb{T} \times \mathbb{E}^{*} \mapsto \mathbb{E}$ fabrique une union taggée à partir d'un tag.

$\text{Match} : \mathbb{E} \times (\mathbb{T} \times \mathbb{V}^{*} \times \mathbb{E})^{*} \times \mathbb{E} \mapsto \mathbb{E}$ filtre une union taggée.

## Analyseur sémantique

Toutes les variables sont alpha-converties et conservées dans une table des symboles. Les variables libres dans le programme sont autorisées, également alpha-converties et ajoutées dans une table à part. L'acceptation de variables libres dans le programme permet à mes yeux de faciliter la gestion du non-déterminisme et d'éviter toute ambiguïté lors de l'analyse. En effet les entrées-sorties peuvent être vues comme des variables libres qui ne sont connues qu'au moment de l'exécution du programme, ce qui permet de s'assurer par exemple qu'un affichage sur la sortie ne serait pas optimisé (de la même manière je me pose la question à savoir si la mémoire, dans le cas où je traiterais les effets de bord, peut être modélisée comme une variable libre, ce qui expliciterait le non-déterminisme des effets de bord).

$\mathbb{E}_{ast} \times \left( \mathbb{V}_{ast} \mapsto \mathbb{V} \right) \times \left( \mathbb{T}_{ast} \mapsto \mathbb{T} \right) \vdash_{\text{ast'}} \mathbb{E} \times \left( \mathbb{V} \mapsto \mathbb{V}_{ast} \right) \times \left( \mathbb{V}_{ast} \mapsto \mathbb{V} \right)$

$e ~ A ~ C \vdash_{\text{ast'}} e' ~ S ~ L$

- $e$ est l'expression AST à compiler ;
- $A$ est la table des abstractions existantes dans la portée de $e$ ;
- $C$ est la table des constructeurs dans la portée de $e$ ;
- $e'$ est l'expression générée ;
- $S$ est la table des variables substituées dans $e'$ ;
- $L$ est la table des variables libres de $e$.

Par la suite, je note $\overline{x}$ l'identifiant unique donné à une variable $x$, $e'$ l'expression générée par l'expression $e$, $\emptyset$ une table vide, $x \coloneqq y$ une entrée de table dont l'étiquette est $x$ et la valeur est $y$ et $X \sqcup Y$ l'union de deux tables disjointes.

\begin{gather}
   \tag{Int}
   \over \left( \text{Int} ~ i \right) A ~ C \vdash_{\text{ast'}} \left( \text{Int} ~ i \right) \emptyset ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Binary}
   \begin{split}
      e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      e_2 \left( A \sqcup L_{e_1} \right) C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ L_{e_2}
   \end{split}
   \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) A ~ C \vdash_{\text{ast'}} \left( \text{Binary} ~ \diamond ~ e_1' ~ e_2' \right) \left( S_{e_1} \sqcup S_{e_2} \right) \left( L_{e_1} \sqcup L_{e_1} \right)
\end{gather}
\begin{gather}
   \tag{Fun}
   \begin{split}
      e \left( A \sqcup \lbrace x \coloneqq \overline{x} \rbrace \right) C &\vdash_{\text{ast'}} e' ~ S_{e} ~ L_{e}
   \end{split}
   \over \left( \text{Fun} ~ x ~ e \right) A ~ C \vdash_{\text{ast'}} \left( \text{Fun} ~ \overline{x} ~ e' \right) \left( S_{e} \sqcup \lbrace \overline{x} \coloneqq x \rbrace \right) L_{e}
\end{gather}
\begin{gather}
   \tag{Var1}
   \begin{split}
      x \in \mathcal{D}(A)
   \end{split}
   \over \left( \text{Var} ~ x \right) A ~ C \vdash_{\text{ast'}} \left( \text{Var} ~ A\left( x \right) \right) \emptyset ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Var2}
   \begin{split}
      x \notin \mathcal{D}(A)
   \end{split}
   \over \left( \text{Var} ~ x \right) A ~ C \vdash_{\text{ast'}} \left( \text{Var} ~ \overline{x} \right) \emptyset ~ \lbrace x \coloneqq \overline{x} \rbrace
\end{gather}
\begin{gather}
   \tag{Let}
   \begin{split}
      e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      e_2 \left( A \sqcup L_{e_1} \sqcup \lbrace x \coloneqq \overline{x} \rbrace \right) C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ L_{e_2}
   \end{split}
   \over \left( \text{Let} ~ x ~ e_1 ~ e_2 \right) A ~ C \vdash_{\text{ast'}} \left( \text{Let} ~ \overline{x} ~ e_1' ~ e_2' \right) \left( S_{e_1} \sqcup S_{e_2} \sqcup \lbrace \overline{x} \coloneqq x \rbrace \right) \left( L_{e_1} \sqcup L_{e_2} \right)
\end{gather}
\begin{gather}
   \tag{If}
   \begin{split}
      e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      e_2 \left( A \sqcup L_{e_1} \right) C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ L_{e_2} \\
      e_3 \left( A \sqcup L_{e_1} \sqcup L_{e_2} \right) C &\vdash_{\text{ast'}} e_3' ~ S_{e_3} ~ L_{e_3}
   \end{split}
   \over \left( \text{If} ~ e_1 ~ e_2 ~ e_3 \right) A ~ C \vdash_{\text{ast'}} \left( \text{Binary} ~ e_1' ~ e_2' ~ e_3' \right) \left( S_{e_1} \sqcup S_{e_2} \sqcup S_{e_3} \right) \left( L_{e_1} \sqcup L_{e_1} \sqcup L_{e_3} \right)
\end{gather}
\begin{gather}
   \tag{App}
   \begin{split}
      e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      e_2 \left( A \sqcup L_{e_1} \right) C &\vdash_{\text{ast'}} e_2' ~ S_{e_2} ~ L_{e_2}
   \end{split}
   \over \left( \text{App} ~ e_1 ~ e_2 \right) A ~ C \vdash_{\text{ast'}} \left( \text{App} ~ e_1' ~ e_2' \right) \left( S_{e_1} \sqcup S_{e_2} \right) \left( L_{e_1} \sqcup L_{e_2} \right)
\end{gather}
\begin{gather}
   \tag{Type}
   \begin{split}
      e ~ A \left( C \sqcup \lbrace t_i \coloneqq i, i \in 1 \dots n \rbrace \right) &\vdash_{\text{ast'}} e' ~ S_{e} ~ L_{e}
   \end{split}
   \over \left( \text{Type} ~ s \left( t_i, s_i \right)_{i=1}^{i=n} ~ e \right) A ~ C \vdash_{\text{ast'}} e' ~ S_{e} ~ L_{e}
\end{gather}
\begin{gather}
   \tag{Constructor}
   \begin{split}
      e_1 ~ A ~ C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      \dots \\
      e_n \left( \bigsqcup_{i=1}^{i=n-1} S_{e_{i-1}} \sqcup A \right) C &\vdash_{\text{ast'}} e_n' ~ S_{e_n} ~ L_{e_n}
   \end{split}
   \over \left( \text{Constructor} ~ s \left( e_i \right)_{i=1}^{i=n} \right) A ~ C \vdash_{\text{ast'}} \left( \text{Constructor} \left( C(s) \right) \left( e_i' \right)_{i=1}^{i=n} \right) \left( \bigsqcup_{i=1}^{i=n} S_{e_i} \right) \left( \bigsqcup_{i=1}^{i=n} L_{e_i} \right)
\end{gather}
\begin{gather}
   \tag{LetRec}
   \begin{split}
      e_1 \left( \lbrace x_i \coloneqq x_i', i \in 1 \dots n \rbrace \sqcup A \right) C &\vdash_{\text{ast'}} e_1' ~ S_{e_1} ~ L_{e_1} \\
      \dots \\
      e_n \left( \bigsqcup_{i=1}^{i=n-1} S_{e_{i-1}} \sqcup \lbrace x_i \coloneqq x_i', i \in 1 \dots n \rbrace \sqcup A \right) C &\vdash_{\text{ast'}} e_n' ~ S_{e_n} ~ L_{e_n}
   \end{split}
   \over \left( \text{LetRec} \left( x_i, e_i \right)_{i=1}^{i=n} ~ e \right) A ~ C \vdash_{\text{ast'}} \left( \text{LetRec} \left( x_i', e_i' \right)_{i=1}^{i=n} ~ e' \right) \left( \bigsqcup_{i=1}^{i=n} S_{e_i} \right) \left( \bigsqcup_{i=1}^{i=n} L_{e_i} \right)
\end{gather}
\begin{gather}
   \tag{Match}
   \begin{split}
   (todo)
   \end{split}
   \over \left( \text{Match} ~ e \left( m_i, e_i \right)_{i=1}^{i=n} \right) A ~ C \vdash_{\text{ast'}} \left( \text{Match} ~ e' \left( t_i', \left( a_i^j \right)_{j=1}^{j=m_i}, e_i' \right)_{i=1}^{i=n}\right) \left( \bigsqcup_{i=1}^{i=n} S_{e_i} \sqcup S_{e} \right) \left( \bigsqcup_{i=1}^{i=n} L_{e_i} \sqcup L_{e} \right)
\end{gather}

\newpage

# Analyse du flot de contrôle

La conversion CPS/CFG transforme l'AST' en un ensemble de basic blocs. A l'origine il s'agissait d'une conversion CPS, mais l'explicitation des variables libres et la décontextualisation des blocs fait qu'aujourd'hui elle ressemble davantage à une conversion vers un CFG. L'idée est de perdre le moins d'informations possible du programme source tout en ayant sous la main un langage intermédiaire qui permette une analyse simple et puissante.

## Graphe de flot de contrôle

La différence notable avec l'AST' est l'apparition des blocs et l'explicitation des variables libres.

### Identificateurs

On retrouve les identificateurs pour les variables et les tags, auxquels s'ajoute un identificateur pour les pointeurs.

$\mathbb{V} \coloneqq \mathbb{N}$ (variables)

$\mathbb{T} \coloneqq \mathbb{N}$ (tags)

$\mathbb{P} \coloneqq \mathbb{N}$ (pointeurs)

### Expressions

Les expressions correspondent aux instructions élémentaires qui construisent des valeurs.

$\text{Int} : \mathbb{Z} \mapsto \mathbb{E}$ génère un entier.

$\text{Var} : \mathbb{V} \mapsto \mathbb{E}$ crée un alias de variable.

$\text{Add} : \mathbb{V} \times \mathbb{V} \mapsto \mathbb{E}$ additionne deux entiers.

$\text{Sub} : \mathbb{V} \times \mathbb{V} \mapsto \mathbb{E}$ soustrait deux entiers.

$\text{Closure} : \mathbb{P} \times \mathbb{V}^{*} \mapsto \mathbb{E}$ fabrique une fermeture.

$\text{Constructor} : \mathbb{T} \times \mathbb{V}^{*} \mapsto \mathbb{E}$ fabrique une union taggée.

### Instructions

Une instruction est soit une déclaration soit un branchement. Une déclaration construit une valeur à partir d'une expression et l'associe à un identifiant unique. Les valeurs ne peuvent être construites qu'à partir de constantes ou identifiants. Un branchement représente le transfert d'un basic block à un autre que ce soit par le biais d'un appel (fermeture), d'un retour de fonction (return) ou d'un saut conditionnel (filtrage par motif). Comme chaque bloc explicite ses variables libres (arguments), celles-ci doivent apparaître dans les branchements. Les variables libres à destination de différents blocs ne sont jamais réunies afin de conserver un maximum d'informations sur leurs origines.

$\text{Let} : \mathbb{V} \times \mathbb{E} \times \mathbb{I} \mapsto \mathbb{I}$ assigne le résultat d'une expression à une variable.

$\text{Call} : \mathbb{V} \times \mathbb{V}^{*} \times \left( \mathbb{P} \times \mathcal{P}(\mathbb{V}) \right) \mapsto \mathbb{I}$ branche vers le bloc avec l'environnement contenu dans la fermeture puis continue l'éxécution avec le contexte spécifié.

$\text{CallDirect} : \mathbb{P} \times \mathbb{V} \times \mathbb{V}^{*} \times \left( \mathbb{P} \times \mathcal{P}(\mathbb{V}) \right) \mapsto \mathbb{I}$ est identique à $\text{Call}$ à l'exception que le pointeur du bloc vers lequel brancher est connu suite à une étape d'analyse (cette instruction n'est par conséquent jamais générée depuis l'AST').

$\text{If} : \mathbb{V} \times (\mathbb{P} \times \mathcal{P}(\mathbb{V})) \times (\mathbb{P} \times \mathcal{P}(\mathbb{V})) \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{I}$ branche dans le premier bloc si la valeur de la condition est différente de 0, dans le deuxième sinon. Les variables libres (dernier paramètre) sont implicitement passées aux deux branches et seront utilisées par le bloc qui sera éxécuté après.

$\text{MatchPattern} : \mathbb{V} \times (\mathbb{T} \times \mathbb{V}^{*} \times \mathbb{P} \times \mathcal{P}(\mathbb{V}))^{*} \times (\mathbb{P} \times \mathcal{P}(\mathbb{V})) \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{I}$ branche soit vers un des blocs lorsque la valeur de la condition correspond au motif de l'un d'eux, soit vers le bloc par défaut. Les variables libres (dernier paramètre) sont implicitement passées à toutes les branches et seront utilisées par le bloc qui sera éxécuté après.

$\text{Return} : \mathbb{V} \mapsto \mathbb{I}$ retourne la valeur de cette variable au bloc appelant depuis une fermeture.

$\text{IfReturn} : \mathbb{P} \times \mathbb{V} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{I}$ retourne la valeur de cette variable au bloc appelant depuis un branchement conditionnel.

$\text{MatchReturn} : \mathbb{P} \times \mathbb{V} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{I}$ retourne la valeur de cette variable au bloc appelant depuis une branche d'un filtrage par motif.

$\text{ApplyBlock} : \mathbb{P} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{I}$ est un reliquat utilisé uniquement par LetRec.

### Blocs

Chaque bloc est défini différemment en fonction de l'expression à partir de laquelle il est construit (fermeture, retour de fonction ou saut conditionnel), est clos (il explicite les arguments dont il a besoin) et contient une suite de déclarations de variables suivies d'une instruction de branchement. Le dernier argument correspond toujours aux variables libres du bloc (environnement dans le cas d'une fermeture).

$\text{Clos} : \mathbb{V}^{*} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ est une fermeture avec ses arguments.

$\text{Return} : \mathbb{V} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ sera éxécuté après un appel de fermeture avec comme argument son résultat.

$\text{IfBranch} : \mathcal{P}(\mathbb{V}) \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ est un branchement conditionnel avec les variables libres qui seront passées au bloc éxécuté ensuite.

$\text{IfJoin} : \mathbb{V} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ sera éxécuté après un branchement conditionnel avec comme argument son résultat.

$\text{MatchBranch} : \mathbb{V}^{*} \times \mathcal{P}(\mathbb{V}) \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ est un branchement d'un filtrage par motif avec ses arguments (charge du constructeur ou aucun pour le branchement par défaut) et les variables libres qui seront passées au bloc éxécuté ensuite.

$\text{MatchJoin} : \mathbb{V} \times \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ sera éxécuté après un branchement d'un filtrage par motif avec comme argument son résultat.

$\text{Cont} : \mathcal{P}(\mathbb{V}) \mapsto \mathbb{B}$ est un reliquat utilisé uniquement par LetRec.

### Ensemble de blocs

$blocks \coloneqq \mathbb{P} \mapsto (\mathbb{B} \times \mathbb{I})$

## Conversion CFG

L'algorithme a la signature suivante : $\mathbb{E}_{ast'} \times \mathbb{V} \times \mathbb{B}^{*} \times \mathbb{E} \vdash_{\text{cfg}} \mathbb{E} \times \mathbb{V}^{*} \times blocks$.

$e ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} e' ~ V_e ~ B_e$

- $e$ est l'expression sous forme d'AST' à intégrer au CFG ;
- $v$ est le nom de la variable dans lequel conserver le résultat de l'évaluation de $e$ ;
- $V$ est l'ensemble des variables libres (arguments) de $\epsilon$ devant être sauvegardées durant l'évaluation de $e$ pour être réstaurées après (ne doit pas contenir $v$).
- $\epsilon$ est l'expression déjà transpilée au format CFG qui sera éxécutée après $e$. Elle est supposée faire usage de $v$ et chaque variable libre qui y apparaît doit figurer dans $V$ ;
- $e'$ est $e$ transpilée ;
- $V_e$ est l'ensemble des variables libres apparaissant dans $e$ qui ne proviennent pas de $V$. En théorie, les variables libres de $e'$ sont exactement $V \cup V_e$. A l'origne $V_e$ contenait toutes les variables libres apparaissant dans $e'$ de la même manière que $V$ contient toutes les variables libres de $\epsilon$ mais j'ai amendé cela pour simplifier l'implémentation. Cette nouvelle version est néanmoins bancale ($e'$ peut ne pas contenir $\epsilon$ mais un appel vers un bloc contenant $\epsilon$ et il est impossible de le déduire d'après $V_e$) c'est pour cela que j'ai prévu de revenir à l'ancienne version quand j'aurai la garantie qu'elle est toujours compatible ;
- $B_e$ est l'ensemble des blocs générés par la transpilation de $e$.

### Conventions

Je note $\sigma = \text{Expression}; \epsilon$ pour simplifier les déclarations, qui est l'équivalent de $\text{Let} ~ \sigma ~ \text{Expression} ~ \epsilon$.

- $\overline{e}$ correspond à l'identifiant unique (variable) attribué au résultat de l'évaluation de $e$.
- $\dot{e}$ correspond à l'identifiant unique (pointeur)  attribué au bloc qui contiendra $e$.

### Algorithme

\begin{gather}
   \tag{Int}
   \over \left( \text{Int} ~ i \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} \left( v = \text{Int} ~ i; \epsilon \right) \emptyset ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Var}
   \label{Var}
   \over \left( \text{Var} ~ x \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} \left( v = x; \epsilon \right) \lbrace x \rbrace ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Let}
   \begin{split}
      e_2 ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} e_2' ~ V_{e_2} ~ B_{e_2} \quad V_3 = V_{e_2} \setminus \lbrace x \rbrace \\
      e_1 ~ x \left( V \cup V_3 \right) e_2' \vdash_{\text{cfg}} e_1' ~ V_{e_1} ~ B_{e_1}
   \end{split}
   \over \left( \text{Let} ~ x ~ e_1 ~ e_2 \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} e_1' \left( V_3 \cup V_{e_1} \right) \left( B_{e_1} \sqcup B_{e_2} \right)
\end{gather}
\begin{gather}
\tag{Binary}
   \begin{split}
      e_2 ~ \overline{e_2} \left( V \cup \lbrace \sigma_1 \rbrace \right) (v = \overline{e_1} \diamond \overline{e_2}; \epsilon) &\vdash_{\text{cfg}} e_2' ~ V_{e_2} ~ B_{e_2} \\
      e_1 ~ \overline{e_1} ~ (V_{e_2} \cup V) ~ e_2' &\vdash_{\text{cfg}} e_1' ~ V_{e_1} ~ B_{e_1}
   \end{split}
   \over \left( \text{Binary} ~ \diamond ~ e_1 ~ e_2 \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} e_1' \left( V_{e_1} \cup V_{e_2} \right) \left( B_{e_1} \sqcup B_{e_2} \right)
\end{gather}
\begin{gather}
   \tag{Fun}
   \begin{split}
      e ~ \overline{e} ~ \emptyset \left( \text{Return} ~ \overline{e} \right) \vdash_{\text{cfg}} e' ~ V_{e} ~ B_{e} \quad V_2 = V_{e} \setminus \lbrace x \rbrace
   \end{split}
   \over \left( \text{Fun} ~ x ~ e \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} \left( v = \text{Closure} ~ \dot{e} ~ V_2; \epsilon \right) V_2 \left( B_{e} \sqcup \lbrace \dot{e} = \text{Clos} \left( x \right) V_2 ~ e' \rbrace \right)
\end{gather}
\begin{gather}
   \tag{If}
   \begin{split}
      e_2 ~ \overline{e_2} ~ V ~ (\text{Ifreturn} ~ \dot{e_1} ~ \overline{e_2} ~ V) &\vdash_{\text{cfg}} e_2' ~ V_{e_2} ~ B_{e_2} \\
      e_3 ~ \overline{e_3} ~ V ~ (\text{Ifreturn} ~ \dot{e_1} ~ \overline{e_3} ~ V) &\vdash_{\text{cfg}} e_3' ~ V_{e_3} ~ B_{e_3} \\
      e_1 ~ \overline{e_1} ~ (V \cup V_{e_2} \cup V_{e_3}) ~ (\text{If} ~ \overline{e_1} ~ \dot{e_2} ~ V_{e_2} ~ \dot{e_3} ~ V_{e_3} ~ V) &\vdash_{\text{cfg}} e_1' ~ V_{e_1} ~ B_{e_1} \\
      B_{e_1e_2e_3} = \lbrace \dot{e_1} = \text{Ifjoin} ~ v ~ V ~ \epsilon, \dot{e_2} = \text{Ifbranch} ~ V_{e_2} ~ V ~ e_2', \dot{e_3} = \text{Ifbranch} ~ V_{e_3} ~ V ~ e_3' \rbrace
   \end{split}
   \over (\text{If} ~ e_1 ~ e_2 ~ e_3) ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} e_1' ~ (V_{e_1} \cup V_{e_2} \cup V_{e_3}) \left( B_{e_1} \sqcup B_{e_2} \sqcup B_{e_3} \sqcup B_{e_1e_2e_3} \right)
\end{gather}
\begin{gather}
   \tag{App}
   \begin{split}
      e_2 ~ \overline{e_2} \left( V \cup \lbrace \overline{e_1} \rbrace \right) \left( \text{Call} ~ \overline{e_1} \left( \overline{e_2} \right) \dot{e} ~ V \right) &\vdash_{\text{cfg}} \epsilon_2 ~ V_{e_2} ~ B_{e_2} \\
      e_1 ~ \overline{e_1} \left( V_{e_2} \cup V \right) \epsilon_2 &\vdash_{\text{cfg}} \epsilon_1 ~ V_{e_1} ~ B_{e_1}
   \end{split}
   \over \left( \text{App} ~ e_1 ~ e_2 \right) v ~ V ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 \left( V_{e_1} \cup V_{e_2} \right) \left( B_{e_1} \sqcup B_{e_2} \sqcup \lbrace \dot{e} = \text{Return} ~ v ~ V ~ \epsilon \rbrace \right)
\end{gather}
\begin{gather}
   \tag{Constructor} f = \begin{cases} f_0 =
   (\text{Constructor} ~ t ~ \alpha) ~ V ~ B \\
   f_n = {f_{n-1} = \epsilon_{n-1} ~ V_{n-1} ~ B_{n-1}
      \quad a_n ~ \alpha_n ~ \alpha \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon_n ~ V_n ~ B_n
      \over \epsilon_n ~ V_n \cup V_{n-1} \setminus \lbrace \alpha_n \rbrace ~ B_n \cup B_{n-1}}
   \end{cases}
   \over (\text{Constructor} ~ t ~ a_n) ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} f_n
\end{gather}
\begin{gather}
   \begin{cases}
      \epsilon_0 = (v = \text{Constructor} ~ t ~ (\overline{a_1} \dots \overline{a_n}); \epsilon) \\
      \epsilon_n =
         { a_n ~ \overline{a_n} ~ V \cup V_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ V ~ B
         \over \epsilon }
   \end{cases}
   \begin{cases}
      V_0 = \lbrace \overline{a_1}, \dots, \overline{a_n} \rbrace \\
      V_n =
         { a_n ~ \overline{a_n} ~ V \cup V_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ V ~ B
         \over V \cup V_{n-1} \setminus \lbrace \overline{a_n} \rbrace }
   \end{cases}
   \begin{cases}
      B_0 = \emptyset \\
      B_n =
         { a_n ~ \overline{a_n} ~ V \cup V_{n-1} \setminus \lbrace a_n \rbrace ~ \epsilon_{n-1} \vdash_{\text{cfg}} \epsilon ~ V ~ B
         \over B \cup B_{n-1} }
   \end{cases}
   \over (\text{Constructor} ~ t ~ (a_1 \dots a_n)) ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} \epsilon_n ~ V_n ~ B_n
\end{gather}
\begin{gather}
   \tag{Match}
   \begin{split}
      d ~ \overline{d} ~ V ~ (\text{Matchreturn} ~ \dot{\epsilon} ~ \overline{d} ~ V) \vdash_{\text{cfg}} \epsilon_d ~ V_d ~ B_d \\
      \begin{cases}
         B_0 = \emptyset \\
            B_n =
         { e_n ~ \overline{e_n} ~ V ~ (\text{Matchreturn} ~ \dot{\epsilon} ~ \overline{e_n} ~ V) \vdash_{\text{cfg}} \epsilon ~ V_{e_n} ~ B
         \quad V_{e_n} = V_{e_n} \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace
         \over B \cup B_{n-1} \cup \lbrace \langle \dot{e_n}, \text{Matchbranch} \left( a_n^1 \dots a_n^{m_n} \right) V_{e_n} ~ V, \epsilon \rangle \rbrace }
      \end{cases} \\
      { V_{e_n} =
      e_n ~ \overline{e_n} ~ V ~ (\text{Matchreturn} ~ \dot{\epsilon} ~ \overline{e_n} ~ V) \vdash_{\text{cfg}} \epsilon ~ V ~ B
      \over V \setminus \lbrace a_n^1, \dots, a_n^{m_n} \rbrace } \\
      e ~ \sigma_e \left( \bigcup_{i=1}^{n} V_{e_i} \cup V_d \cup V \right) \left( \text{Matchpattern} ~ \sigma_e \left( \langle t_i, \dot{e_i}, \left( a_i^j \right)^{j=1 \dots m_i}, V_{e_i} \rangle \right)^{i=1 \dots n} ~ \langle \dot{d}, V_d \rangle ~ V \right) \vdash_{\text{cfg}} \epsilon_1 ~ V_1 ~ B_1
   \end{split}
   \over (\text{Match} ~ e \left( \langle t_i, \left( a_i^j \right)^{j=1 \dots m_i}, e_i \rangle \right)^{i=1 \dots n} ~ d) ~ v ~ V ~ \epsilon \vdash_{\text{cfg}} \epsilon_2 ~ (V_1 \cup V_2) ~ (B_1 \cup B_2)[\dot{e} = \text{Return} ~ v ~ V ~ \epsilon]
\end{gather}


## Nettoyage des alias

Le code CFG ainsi généré est susceptible de contenir de nombreux alias de variables (créés par la règle $\eqref{Var}$) et cela peut nuire à la qualité de l'analyse. La passe de nettoyage supprime tous les alias. A ma connaissance il n'existe pas de méthode simple pour éliminer les alias directement lors de la génération et ainsi se passer de $\text{Var}$ dans le langage intermédiaire.

## Taille

Déterminer la taille du CFG est nécessaire pour certaines heuristiques. Comme le CFG n'est pas le langage qui sera compilé ou éxécuté, le calcul de cette taille est seulement indicatif et n'est en aucun cas précis. La taille du CFG correspond à la somme de la taille de tous ses blocs. La taille d'un bloc correspond au nombre de ses paramètres plus la somme de la taille de ses instructions. La taille d'une instruction dépend principalement du nombre de ses arguments.

## Recensement des variables et appels de blocs

Plusieurs optimisations ont besoin de connaître le nombre de fois qu'une variable est utilisée ou qu'un bloc est appelé. Un tel algorithme de recensement semble trivial mais en fait ne l'est pas du tout. Mon implémentation actuelle ne prend pas en compte la vivacité des blocs, c'est pour cela qu'un bloc appelé par par un bloc mort sera considéré vivant. De plus, étant donné qu'il est impossible de déterminer sans une analyse complète quel bloc sera appelé par un appel indirect, je me contente de supposer un appel à chaque création de fermeture. Il est évident que ces erreurs doivent être corrigées, mais cela ne peut pas se faire sans une approche globale des opimisations qui suivent, en particulier le nettoyage des variables inutilisées et des blocs morts.

## Spécialisation

La spécialisation consiste à copier des blocs. Les appels directs vers les blocs concernés reçoivent un nouveau pointeur vers un bloc fraîchement copié. La copie d'un bloc a pour effet immédiat d'améliorer la précision de l'analyse, et c'est également la première étape de l'inlining.

### Algorithme de spécialisation

L'implémentation actuelle de la spécialisation des blocs n'est pas poussée à son maximum. Les blocs à copier sont toujours copiés indépendamment de l'appel. Il serait intéressant d'intégrer la possibilité de choisir les appels à spécialiser, autrement dit de traiter des couples bloc appelant/bloc appelé. Néammoins un tel couple me paraîtrait insuffisant notamment lorsqu'un même bloc peut être appelé de différentes manières par une même instruction (en théorie c'est le cas du filtrage par motif même si mon implémentation actuelle ne le permet pas). Une solution possible serait de choisir dynamiquement lors de l'analyse de spécialiser tel ou tel appel. Cette solution apporterait certes des réponses mais apporterait probablement de trop nombreux problèmes.

Lors de la copie d'un bloc il est évidemment indispensable de conserver les invariants qui s'appliquent au CFG, en particulier l'unicité des noms de variable, c'est pour cela que chaque copie s'accompagne d'une nouvelle étape de renommage.

### Choix des blocs à spécialiser

Les blocs à spécialiser sont actuellement sélectionnés uniquement selon que leur taille se trouve ou non sous un certain seuil statique. C'est en particulier sur ces choix que je vais être amené à trouver des heuristiques pertinentes d'ici la fin du stage. De telles heuristiques devraient prendre en compte les gains et coûts potentiels apportés par la spécialisation en se basant sur les informations issues de l'analyse.

## Analyse

L'analyse est l'étape la plus compliquée et probablement la plus importante pour permettre d'inliner de manière efficace. L'objectif principal est de transformer au mieux les sauts indirects (appels de fonctions) en sauts directs afin d'être capable d'inliner de tels sauts. Ensuite, même si ce n'est pas obligatoire, il est intéressant de disposer d'une analyse des valeurs assez précise pour se faire une idée de quand inliner pour obtenir les meilleurs bénéfices. Cette analyse s'effectue au niveau du CFG afin d'exploiter la sémantique du langage (en conservant certaines relations) tout en disposant des informations nécessaires sur les blocs.

### Zones d'allocation

Sur conseil de mon tuteur, je réalise une analyse par zone d'allocation. Ce choix donne des garanties de terminaison (il existe un nombre fini de points d'allocation dans le programme) tout en permettant une analyse poussée qui autorise par exemple la récursivité lors de la construction des blocs (fondamental pour traiter les listes). Il y a néanmoins certaines limitations à utiliser une telle analyse. La première que j'ai rencontrée était dûe à la présence de nombreux alias de variables présents dans le code CFG généré ce qui réduisait grandement la précision de l'analyse. Problème corrigé en effectuant une passe de nettoyage des alias avant chaque tour d'analyse.

#### Paramètres

Chaque paramètre de bloc est identifié par un ensemble de point d'allocation correspondant aux endroits d'où peut avoir été déclarée et initialisée sa valeur.

#### Point d'allocation

Un point d'allocation correspond simplement à une déclaration, c'est à dire une instruction $\text{Let}$. Etant donné que chaque valeur créée est déclarée (il n'existe pas de valeur temporaire) avec un nom de variable unique (garanti par l'alpha-conversion), un point d'allocation peut donc être identifé par le nom de variable utilisé par $\text{Let}$. 


  contient une valeur abstraite correspondant . 

> 





Une contrainte importante portée sur l'analyse est la nécessité de pouvoir réaliser plusieurs analyses consécutives afin de pouvoir comparer les performances et résultats d'une seule analyse en profondeur face à plusieurs petites analyses. Cela implique que le langage intermédiaire sur lequel s'effectue l'analyse (en l'occurence ici le CFG) doit rester le même après analyse. C'est pour cette raison que les résultats éventuels

### Valeurs

Actuellement seulement deux domaines abstraits sont nécessaires pour représenter toutes les valeurs du langage.

#### Entiers

Les entiers sont représentés de la manière la plus simple qui soit, c'est à dire des singletons munis de Top ($Z$).

$\text{Top} : int_d$

$\text{Singleton} : int \rightarrow int_d$

L'union de deux entiers donne toujours Top sauf lorsqu'il s'agit de deux singletons de même valeur.

$u = v \Rightarrow \lbrace u \rbrace \cup_{int_d} \lbrace v \rbrace = \lbrace u \rbrace$

$x \cup_{int_d} y = Top$

#### Fermetures

Le domaine pour les fermetures (resp. les unions taggées) est un environnement d'identifiant vers contexte, où l'identifiant correspond au pointeur de fonction (resp. au tag), et le contexte correspond aux variables libres (resp. au contenu de l'union). Étant donné que les pointeurs de fonctions, les tags ainsi que les contextes (ensemble de zones d'allocations) sont des ensembles bornés par la taille du programme, l'union de deux abstractions est garantie de converger.

$closure_d \coloneqq pointer \rightarrow \mathcal{P}(var)^{*}$

$constructor_d \coloneqq tag \rightarrow \mathcal{P}(var)^{*}$

En pratique, $pointer = tag = int$, c'est pour cela que j'utilise le même domaine pour les constructeurs et les tags.

L'union de deux domaines de fermetures consiste à conserver les entrées distinctes et d'unir les valeurs des entrées communes.

> Deux entrées communes sont censées avoir le même nombre de valeurs.

$$x \cup_{closure} y = \forall z \in \mathcal{D}(x) \cup \mathcal{D}(y),
   \begin{cases}
      \left( x(z)\_i \cup y(z)\_i \right)_{i=1}^{i=n} \text{ si } z \in \mathcal{D}(x) \text{ et } z \in \mathcal{D}(y) \text{ et } |x(z)| = |y(z)| = n \\
      x(z) \text{ si } z \in \mathcal{D}(x) \\
      y(z) \text{ si } z \in \mathcal{D}(y)
   \end{cases}$$

Une valeur abstraite est soit un entier soit une fermeture.

$\text{IntDomain} : int_d \rightarrow value_domain$

$\text{ClosureDomain} : closure_d \rightarrow value_domain$

Comme chaque valeur est toujours initialisée et déclarée avec un identifiant unique, elles sont conservées dans une table d'association correspondant aux allocations.

$allocations \coloneqq var \rightarrow value_domain$


Lors de l'analyse, les blocs conserveront désormais un ensemble de points d'allocations pour chacune de ses variables.

$cont_type \coloneqq \text{block}[var/\mathcal{P}(var)]$

Une frame correspond à un étage de la pile, c'est à dire le pointer vers un bloc qui sera éxécuté au prochain retour d'appel avec les paramètres qui ont été sauvegardés.

$frame \coloneqq pointer \times (\mathcal{P}(var))^{*}$

La pile d'appel est une liste ordonnée d'appels.

$stack_allocs \coloneqq frame^{*}$

Un bloc à analyser est ientifié par son pointeur, ses arguments, la pile d'appel et enfin au contexte d'allocations du moment où il est appelé.

$bbloc \coloneqq pointer \times cont_type \times stack_allocs \times allocations$

Une fonction qui réduit la taille de la pile.

$stack_reduce \coloneqq stack_allocs \rightarrow stack_allocs$

Le contexte d'appel contient la pile d'appel et les arguments d'appel.

> Il est important de noter que si l'on suppose la pile d'appel comme ayant une taille finie (garantie après réduction), la dimension d'un contexte est bornée, ce qui est essentiel pour garantir la terminaison.

$context \coloneqq stack_allocs \times cont_type$

Une table de contextes d'allocations associe un contexte à l'état de ses allocations.

> La dimension d'un contexte étant bornée, les contextes sont donc dénombrables (on peut donc implémenter la table d'association correspondante).

$alloccontexte \coloneqq context \rightarrow allocations$

Une table de contextes de blocs associe chaque bloc à ses contextes d'allocation.

$bloccontexte \coloneqq pointer \rightarrow alloccontexte$

Un appel analysé est une paire associant les paramètres passés avec leurs allocations correspondantes.

$callanalysed \coloneqq cont_type \times allocations$

Un programme analyse (càd le résultat de l'analyse) est une table associant chaque bloc à son appel analysé.

$analysis \coloneqq pointer \rightarrow callanalysed$

### Algorithme d'analyse

$\text{analysis} : bbloc^{*} \times stack_reduce \times blocks \times bloccontexte \rightarrow analysis$

\begin{algorithm}[H]
\DontPrintSemicolon
\SetAlgoLined
\SetKwInOut{Input}{Input}\SetKwInOut{Output}{Output}
\Input{conts reduce prog map}
\BlankLine
\eIf{$conts$ est vide}{
    map2 $\gets \emptyset$\;
    \For{$b \in map$}{
        map2[b] $\gets$ union de toutes les allocations et paramètres\;
    }
    \Return map2
}{
   k, block', stack''', allocations $\gets$ hd(conts)\;
   conts' $\gets$ tl(conts)\;
   stack $\gets$ reduce(stack''')\;

   \eIf{$k \in \mathcal{D}(map)$}{
    oldcontexts $\gets$ map(k)\;

    \eIf{$(stack, block') \in \mathcal{D}(oldcontexts)$}{
        oldallocations $\gets$ oldcontexts((stack, block'))\;
        newallocations $\gets$ oldallocations $\cup$ allocations\;

        \eIf{newallocations = oldallocations}{

        }{
            block, expr $\gets$ prog(k)\;
            nextconts $\gets$ analysiscont(expr, stack''', (blockenv block block'), newallocations)\;
            \Return {analysis(conts'nextconts, reduce, prog, add k (add (stack, block') newallocations oldcontexts) map)}
        }
    }{

    }
   }{

   }
}
\caption{Analyse du programme}
\end{algorithm}

### Abstractions

Actuellement j'utilise deux abstractions pour représenter toutes les valeurs du langage. La première pour les entiers qui est simplement le domaine singleton. La deuxième pour les fermetures (resp. les constructeurs) est un environnement d'identifiant vers contexte, où l'identifiant correspond au pointeur de fonction (resp. au tag), et le contexte correspond aux variables libres (resp. au payload). Étant donné que les pointeurs de fonctions, les tags ainsi que les contextes (ensemble de zones d'allocations) sont des ensembles bornés, l'union de deux abstractions est garantie de converger.

### Preuve de terminaison

Les noms de variables, par extension les zones d'allocations, étant en nombre fini dans le programme, l'ensemble identifiant les valeurs est également fini. De la même manière, un contexte d'appel (ensemble fini de valeurs correspondant aux arguments et pointeur de bloc), est un ensemble fini étant donné que le nombre de blocs dans le programme est également borné. Reste la question épineuse de comment garantir que la pile d'appels ne croît pas infiniment. Afin de tenter d'obtenir une précision maximale, je détecte d'éventuels motifs sur la pile en regardant si 1..N contextes d'appels se répètent et le cas échéant je supprime la répétition. Par exemple la pile d'appels A::B::A::B::C::[] sera remplacée par A::B::C::[]. Après avoir implémenté cette méthode, mes tuteurs m'ont rapidement fait comprendre qu'elle ne pouvait pas garantir la terminaison. Pour la suite du stage je vais certainement devoir durcir la détection de motifs en passant à 0-CFA ou 1-CFA.

### Propagation

La propagation modifie le CFG pour y faire apparaître les résultats de l'analyse. En particulier c'est ici que sont transformés les appels indirects en appels directs.

\newpage

# CFG concret

L'objectif principal de ce langage intermédiaire est d'avoir une représentation bas-niveau stable et facile à interpréter sur laquelle effectuer des benchmarks. L'inlining a lieu sur le CFG concret car les modifications apportées peuvent casser la sémantique d'appel ce qui doit être représenté au niveau de la pile.

## Langage

Le CFG concret est très similaire au CFG, si ce n'est que tous les quasiment tous les traits de langage propres à OCaml ont été concrétisés. A chaque construction de valeur du langage OCaml est associée une structure de données, la plupart d'entre elles devenant des n-uplets. Tous les types de blocs fusionnent en un seul en fixant la sémantique des sauts (passage de l'environnement comme argument) et chaque type de branchement est transformé en un saut (direct ou indirect) avec la possibilité d'ajouter des contextes d'appel sur la pile (seule l'instruction d'appel ajoute un contexte lors de cette transformation).

### Identifiants

On retrouve ici les identifiants du CFG auxquels s'ajoutent de nouveaux identifiants pour gérer les contextes de pile.

$\mathbb{V} \coloneqq \mathbb{N}$ (variables)

$\mathbb{P} \coloneqq \mathbb{N}$ (pointeurs de blocs)

$\mathbb{F} \coloneqq \mathbb{P} \times \mathbb{V}^{*}$ (contexte d'appel)

$\mathbb{S} \coloneqq \mathbb{F}^{*}$ (pile)

### Expressions

De la même manière que pour les identifiants, on retrouve ici la plupart des expressions du CFG. Le principal changement est la transformation des constructeurs et fermetures qui deviennent des n-uplets.

$\text{Const} : \mathbb{Z} \mapsto \mathbb{E}$ génère un entier.

$\text{Pointer} : \mathbb{P} \mapsto \mathbb{E}$ génère un pointeur vers un bloc.

$\text{Var} : \mathbb{V} \mapsto \mathbb{E}$ crée un alias de variable.

$\text{Add} : \mathbb{V} \times \mathbb{V} \mapsto \mathbb{E}$ additionne deux entiers.

$\text{Sub} : \mathbb{V} \times \mathbb{V} \mapsto \mathbb{E}$ soustrait deux entiers.

$\text{Tuple} : \mathbb{V}^{*} \mapsto \mathbb{E}$ fabrique un n-uplet.

$\text{Get} : \mathbb{V} \times \mathbb{N} \mapsto \mathbb{E}$ accède à un champs d'un n-uplet.

### Instructions

Les instructions sont assez similaires, si ce n'est que les branchements permettent désormais d'ajouter des contextes de piles. Le filtrage par motif et le if classique fusionnent en une seule instruction.

$\text{Let} : \mathbb{V} \times \mathbb{E} \times \mathbb{I} \mapsto \mathbb{I}$ assigne le résultat d'une expression à une variable.

$\text{ApplyDirect} : \mathbb{P} \times \mathbb{V}^{*} \times \mathbb{S} \mapsto \mathbb{I}$ réalise un saut direct vers le bloc associé.

$\text{ApplyIndirect} : \mathbb{V} \times \mathbb{V}^{*} \times \mathbb{S} \mapsto \mathbb{I}$ réalise un saut indirect au bloc associé au pointeur contenu dans la variable.

$\text{If} : \mathbb{V} \times (\mathbb{N} \times \mathbb{P} \times \mathbb{V}^{*})^{*} \times (\mathbb{P} \times \mathbb{V}^{*}) \times \mathbb{S} \mapsto \mathbb{I}$ réalise un saut vers le bloc associé à la valeur de la condition ou vers le bloc par défaut.

$\text{Return} : \mathbb{V} \mapsto \mathbb{I}$ retourne le résultat contenu dans la variable.

### Bloc

Il n'existe plus de sémantique pour chaque type de bloc, l'ordre des arguments est maintenant fixé.

$\mathbb{B} \coloneqq \mathbb{V}^{*} \times \mathbb{I}$ (bloc)

### Blocs

Le CFG reste un ensemble de blocs.

$blocks \coloneqq \mathbb{P} \rightarrow \mathbb{B}$

## Génération du CFG concret

La transpilation d'un bloc CFG peut générer plusieurs blocs concrêtisés.

$\vdash_{\text{cfg'}} : \mathbb{B}_{cfg} \times \mathbb{I} \mapsto \mathbb{B} \times blocks$

\begin{gather}
   \tag{Cont}
   \over \left( \text{Cont} ~ args \right) ~ i \vdash_{\text{cfg'}} args ~ i ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Return}
   \over \left( \text{Return} ~ a_0 ~ \left( a_i \right)_{i=1}^{i=n} \right) ~ i \vdash_{\text{cfg'}} \left( a_i \right)_{i=0}^{i=n} ~ i ~ \emptyset
\end{gather}
\begin{gather}
   \tag{Clos}
   \begin{cases}
      i_0 = \text{ApplyDirect} ~ p ~ \left( a_{n+1}, \dots, a_m, a_1, \dots, a_n \right) ~ \left( \right) \\
      i_n = \left( a_n = \text{Get} ~ e ~ n; i_{n-1} \right)
   \end{cases}
   \over \left( \text{Clos} ~ \left( a_i \right)_{i=0}^{i=n} ~ \left( a_i \right)_{i=n+1}^{i=m} \right) ~ i \vdash_{\text{cfg'}} \left( e, a_{n+1}, \dots, a_m \right) ~ i_n ~ \lbrace p = \left( a_{n+1}, \dots, a_m, a_1, \dots, a_n \right), i \rbrace
\end{gather}
\begin{gather}
   \tag{IfBranch}
   \over \left( \text{IfBranch} ~ \left( a_i \right)_{i=0}^{i=n} ~ \left( a_i \right)_{i=n+1}^{i=m} \right) ~ i \vdash_{\text{cfg'}} \left( a_i \right)_{i=0}^{i=m} ~ i ~ \emptyset
\end{gather}
\begin{gather}
   \tag{IfJoin}
   \over \left( \text{IfJoin} ~ a_0 ~ \left( a_i \right)_{i=1}^{i=n} \right) ~ i \vdash_{\text{cfg'}} \left( a_i \right)_{i=0}^{i=n} ~ i ~ \emptyset
\end{gather}
\begin{gather}
   \tag{MatchBranch}
   \begin{cases}
      i_0 = i \\
      i_n = \left( a_n = \text{Get} ~ e ~ n; i_{n-1} \right)
   \end{cases}
   \over \left( \text{MatchBranch} ~ \left( a_i \right)_{i=0}^{i=n} ~ \left( a_i \right)_{i=n+1}^{i=m} ~ \left( a_i \right)_{i=m+1}^{i=o} \right) ~ i \vdash_{\text{cfg'}} \left( e, a_{n+1}, \dots, a_m, a_{m+1}, \dots, a_o \right) ~ i_n ~ \emptyset
\end{gather}
\begin{gather}
   \tag{MatchJoin}
   \over \left( \text{MatchJoin} ~ a_0 ~ \left( a_i \right)_{i=1}^{i=n} \right) ~ i \vdash_{\text{cfg'}} \left( a_i \right)_{i=0}^{i=n} ~ i ~ \emptyset
\end{gather}

### Taille

Calculer la taille du CFG concret est uniquement intéressant d'un point de vue performances et connaître l'impact des différentes optimisations. Le calcul se fait de la même manière que pour le CFG.

### Recensement des variables et appels de blocs

De la même manière que lors de la phase CFG, les variables et les appels de blocs sont recensés afin de supprimer les variables inutilisées et les blocs morts.

## Inlining

Inliner un bloc consiste à intégrer son contenu dans le bloc appelant à la place de la dernière instruction (branchement). Chaque argument du bloc inliné est remplacé par la variable qui lui a été assignée lors du branchement par le bloc appelant. Pour l'instant seuls les appels directs peuvent être inlinés. Si lors de l'appel des contextes étaient empilés sur la pile, alors le branchement du bloc inliné en tiendra compte. En particulier, si le branchement du bloc inliné est un retour de fonction celui-ci dépilera la pile et deviendra un saut direct. Dans les autres cas les contextes du bloc appelant sont empilés sur les contextes du bloc appelé, ce qui permet d'avoir des sauts vers l'intérieur d'une fonction.

> Sont actuellement inlinés tous les blocs appelés exactement 1 fois (les blocs spécialisés sont ainsi tous conernés).

## Interprétation

C'est le CFG concret que j'interprête pour dans un premier temps m'assurer de la validité de toutes les transformations. Pour la suite du stage je serai amené à extraire de nombreuses informations issues de l'interprétation pour vérifier la pertinance des heuristiques mises en place.

\newpage

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

# Conclusion
