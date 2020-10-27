<!--
    To generate the readme, run:

    python -m readme2tex --output README.md .README.md
-->

# 'the chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. We can use first order logic to express constraints.

### Likes
| liker| likee |
|------|--------|
| Matt | John   |
| John  |  John |
| John | Mary   |
### Manage
|  name| manager |
|------|--------|
| Matt | John   |
| John |  John |
| Mary | John   |

We can express that "`Name` is a primary key of `Manage` by <img src="https://rawgit.com/kris-brown/chase/master/.svgs//35cb88e174fb5199c8bb8dd4ab842676.svg?invert_in_darkmode" align=middle width=18.52743585pt height=22.8310566pt/>. We can express that `likee` is a foreign key <img src="https://rawgit.com/kris-brown/chase/master/.svgs//5a86b8ec544eb234ca2251a10ab4bbea.svg?invert_in_darkmode" align=middle width=127.9254438pt height=22.8310566pt/>

<img src="https://rawgit.com/kris-brown/chase/master/.svgs//e2bba634eee9d6889508a8a976961f45.svg?invert_in_darkmode" align=middle width=51.58858815pt height=26.7617526pt/>


<p align="center"><img src="https://rawgit.com/kris-brown/chase/master/.svgs//522cbfbc866df378cb95b2ef083131b2.svg" align=middle width=0.0pt height=0.0pt/></p>

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.