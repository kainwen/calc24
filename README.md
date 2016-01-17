算24点基于Erlang
================

本想写一个教程，还是太懒，半成品在此： https://www.zybuluo.com/kainwen/note/266023


## Run

```bash
make && make run
```

Then in the erlang shell,

```bash
1> calc24:calc24([5,5,5,1]).
( ( 5 - ( 1 / 5 ) ) * 5 )
ok
2> calc24:calc24([3,3,8,8]).
( 8 / ( 3 - ( 8 / 3 ) ) )
ok
```
