<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/chase:/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->

# 'The chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background - relational algebra

Connected with relational databases that you might be more familiar with is the concept of _relational algebra_. What we called tables are now called _relations_, which we can think of as sets of tuples of some fixed length. Importantly we can ask whether a given tuple ![t=(a,b, c, ...)](doc/teximg/tex_img_0_8S501.png) is an element of a given relation ![R](doc/teximg/tex_img_1_D7VUS.png), i.e. is it the case that ![t \in R](doc/teximg/tex_img_2_JLMQG.png)? We can isolate particular attributes of a tuple by projection, which we could use subscripts for: e.g. if the previous tuple was ![t](doc/teximg/tex_img_3_AA7D2.png), then ![t_2 = b](doc/teximg/tex_img_4_EM5SM.png). It's easiest to visualize the relations as tables:

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Likes}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline John            & Mary          \\ \hline\end{tabular}](doc/teximg/tex_img_5_ZGN52.png)

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Manage}   \\ \hline \textbf{Employee}  & \textbf{Manager}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline Mary            & John          \\ \hline\end{tabular}](doc/teximg/tex_img_6_TIPPI.png)

We can use [first-order logic](https://www.javatpoint.com/first-order-logic-in-artificial-intelligence) to express constraints. We can express that "`Employee` is a _primary key_ of `Manage`" by ![\forall x, y \in Manage: x_{Employee}=y_{Employee} \Rightarrow x=y](doc/teximg/tex_img_7_O79UO.png). We can express that "`Likee` is a _foreign key_ ![Likes \rightarrow Manage](doc/teximg/tex_img_8_ULQT2.png)" by writing ![\forall x \in Likes: \exists mgr: (x_{Likee},\ mgr) \in {Manage}](doc/teximg/tex_img_9_BISSC.png)

Now suppose we have a database ![I](doc/teximg/tex_img_10_7SN1H.png) and a set constraints ![\Sigma](doc/teximg/tex_img_11_OCYJL.png), we can ask whether ![I](doc/teximg/tex_img_12_PVYTJ.png) satisfies ![\Sigma](doc/teximg/tex_img_13_DEDC1.png). If not, we can imagine there being the best or nearest-relative of ![I](doc/teximg/tex_img_14_4ZHX2.png) that _does_ satisfy ![\Sigma](doc/teximg/tex_img_15_V429D.png). **The chase** is an algorithm that finds this other database, given ![I](doc/teximg/tex_img_16_DQPIU.png) and ![\Sigma](doc/teximg/tex_img_17_21F0C.png). The remaining background material to cover concerns how to define this notion of a "best" relative of ![I](doc/teximg/tex_img_18_91FKH.png).

## Background - database homomorphisms

One interesting relationship between databases is whether one is a subset of the other.

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{R}   \\ \hline \textbf{1}  & \textbf{2}\\ \hline 200            & 200          \\ \hline\end{tabular}](doc/teximg/tex_img_19_IJI39.png)

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Q} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline 7              & 8        \\ \hline 200            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_20_491XF.png)

We'll consider single-relation database instances in this section to keep the examples small, which allows us to abuse notation and treat the relations as databases in their own right. In the above examples, we could say ![R \subset Q](doc/teximg/tex_img_21_SS910.png) to mean one database is a subset of the other.

Although all example databases above have been fully specified, generally databases in this formalism can have "labeled nulls" which behave like variables.

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{S} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline a            & a        \\ \hline b            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_22_6O82K.png)

This new instance ![S](doc/teximg/tex_img_23_SNQAQ.png) says there's a tuple that has two of the same unknown value, and another which has an unknown first value and ![200](doc/teximg/tex_img_24_5T585.png) for the second value. We need to refine our notion of ![\subset](doc/teximg/tex_img_25_T41JZ.png) in light of the possibility of labeled nulls. The relevant relation now is called a _homomorphism_ (![A \rightarrow B](doc/teximg/tex_img_26_TVJPV.png)) which consists in an assignment to the labeled nulls in ![A](doc/teximg/tex_img_27_IN9VK.png) such that, when substituting ![A](doc/teximg/tex_img_28_7KVY6.png) with this assignment, we get a database instance that is literally a subset of ![B](doc/teximg/tex_img_29_7C577.png). For example, the assignment ![\{a \mapsto 200, b \mapsto 200\}](doc/teximg/tex_img_30_CSQWB.png) is a homomorphism ![S \rightarrow R](doc/teximg/tex_img_31_MQM4N.png). There is no homomorphism ![R \rightarrow S](doc/teximg/tex_img_32_OS4J2.png). (![R \rightarrow Q](doc/teximg/tex_img_33_YABZM.png) is a homomorphism trivially, no substitution needed; for any ![A](doc/teximg/tex_img_34_QZC4F.png) we have ![A \rightarrow A](doc/teximg/tex_img_35_ES2GY.png), given we can make an identity substitution).

Even with merely a simple binary relation like ![\rightarrow](doc/teximg/tex_img_36_W5H47.png), we can derive a notion of the 'closest' related ![U](doc/teximg/tex_img_37_1IAPP.png) for some given ![I](doc/teximg/tex_img_38_XO72O.png). Fix a given ![\Sigma](doc/teximg/tex_img_39_T15P5.png) of constraints and imagine the set of all possible instances which both satisfy ![\Sigma](doc/teximg/tex_img_40_QIITZ.png) and are related to ![I](doc/teximg/tex_img_41_PZ326.png). Among this set, it may be the case that there exists a ![U](doc/teximg/tex_img_42_X7KQY.png) such that ![I \rightarrow U \rightarrow X](doc/teximg/tex_img_43_L94VR.png) for all ![X](doc/teximg/tex_img_44_P2GD1.png) instances in this set, i.e. _any_ homomorphism can be factored through a homomorphism with ![U](doc/teximg/tex_img_45_TX0YC.png).

(Analgously, consider the relation ![\le](doc/teximg/tex_img_46_JFT3V.png) for integers, let ![I=12](doc/teximg/tex_img_47_3SLOO.png) and ![\Sigma = \{> 14, < 20\}](doc/teximg/tex_img_48_C35MY.png) ... we want something that is 'related' to ![I](doc/teximg/tex_img_49_M3EMN.png), so we are looking for an ![i](doc/teximg/tex_img_50_TFG16.png) such that ![12 \le i](doc/teximg/tex_img_51_KZE3T.png) which also satisfies the constraints. There exist 5 possible candidates (![15](doc/teximg/tex_img_52_EDB2D.png) through ![19](doc/teximg/tex_img_53_BQWHG.png) inclusive) and it happens that for any candidate ![X](doc/teximg/tex_img_54_Q4Z1Y.png) we can write ![I \le U \le X](doc/teximg/tex_img_55_EGU7O.png) with the value of ![U=15](doc/teximg/tex_img_56_IU00V.png). Thus, ![15](doc/teximg/tex_img_57_W6YSF.png) is the 'closest' integer to ![12](doc/teximg/tex_img_58_HJ71E.png) that satisfies the constraints.)

The database instance ![U](doc/teximg/tex_img_59_VRME0.png) that satisfies this property is called _universal_ (with respect to ![I,\  \Sigma](doc/teximg/tex_img_60_QH14H.png)), and this is the instance that the chase returns, if it exists.

## Why

Reading about what the chase is and how it works will be more interesting with context as to why finding a universal database instance is of interest.

- Query Optimization
- Constraint Implication
- Universal Solutions / Data Exchange
- Certain Answers

## What

The simplest variation of the chase considers each constraint as a potential trigger which can be _fired_ to produce a new instance that satisfies that constraint. We then iteratively repeat this process until there exist no active triggers (i.e. produce a sequence of instances until we arrive at one which satisfies the constraints).

The literature is generally concerned with two types of constraints.

- Tuple-generating dependencies: _if_ there exist some tuples satisfying such-and-such conditions, _then_ there must exist these other tuples satisfying such-and-such conditions. To _fire_ this constraint is to add the tuples in the consequent clause.
- Equality-generating dependencies: _if_ there exist some tuples (which involve two variables, say ![x](doc/teximg/tex_img_61_H1NGL.png) and ![y](doc/teximg/tex_img_62_3ESJU.png)) satisfying such-and-such conditions, _then_ ![x=y](doc/teximg/tex_img_63_ZP5DX.png). To _fire_ this constraint is to take all instances of ![y](doc/teximg/tex_img_64_W8B12.png) and replace them with ![x](doc/teximg/tex_img_65_HA3BO.png) (or vice-versa). If both ![x](doc/teximg/tex_img_66_Q366X.png) and ![y](doc/teximg/tex_img_67_E3YMB.png) match with constants which have different values, then the chase fails.
- In both cases, there is a clear antecedent condition to check to see if the trigger is _active_.

It's possible for the sequence of instances generated by firing constraints to never terminate. For some simple chase implementations, this doesn't imply that a universal solution doesn't exist. However, more sophisticated implementations (such as the _core_ chase) are complete, meaning they terminate iff there exists a solution.

## Functionality

This repo has implementations in different languages.
| Language | Python | Haskell |
| :-----------: | :-----------: | :----: |
| Run test suite | ✅<br> Run `pytest test_chase.py` | ✅ <br> Run `stack test`|
| Read relations / database instances from CSV | ✅ | ✅ |
| Fire TGD and EGDs | ✅ | ✅ |
| Basic chase algorithm | ✅ |❌|

## References

1. Gösta Grahne and Adrian Onet. _Anatomy of the Chase_. Fundamenta Informaticae. **157** (2018) _221–270_.
2. Alin Deustch, Alan Nash, and Jeff Remmel. _The chase revisited_. PODS (2008).
