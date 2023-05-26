# InnerSource Project Fitness

The InnerSource Project Fitness Tracker is designed to gauge how appropriate a project is for InnerSource.

Assessing the InnerSource-readiness (fitness) of projects can help in selecting projects that have the potential to help demonstrate the power of InnerSource, and with that be great role-models and pilot projects in an InnerSource Program. For further thoughts on the use case for this tool, also read the [Good First Project](https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/1-initial/good-first-project.md) pattern.

The questions used in this tool are motivated by the [InnerSource Checklist](https://innersourcecommons.org/learn/books/understanding-the-innersource-checklist/), 
the [Nine Factor Model for InnerSource adoption](https://ieeexplore.ieee.org/document/6809709), and the 
[State of InnerSource Survey 2020](https://tapjdey.github.io/InnerSource_Survey_2020/index.html).

## How to run it:

1. [Install R](https://www.r-project.org/).
1. Open R terminal, type in: `install.packages('shiny')`
1. Type in: `library(shiny)`
1. Type in: `runGitHub('tapjdey/InnerSource_Project_Fitness', ref = "main")`

## Known Issues:

* Selections will be cleared for the multiple choice question and the Likert scale based questions when changing language.
