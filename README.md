I've been reading Bob Nystrom's excellent [Crafting Interpreters](https://craftinginterpreters.com/) and I felt inspired to try to implement Lox in both Haskell and Rust.  Let's see how much I can do!

# hlox
hlox is the Haskell implementation of lox, and it roughly corresponds to jlox in Crafting Interpreters.
Like jlox, hlox is a tree-walking interpreter which implicitly makes use of the host language's garbage collection to deallocate stale objects.

Since my primary goal is to _learn_, rather than to produce a production-ready interpreter, I am intentionally doing a lot by hand for which any reasonable person would use a pre-canned abstraction.
As perhaps the most salient example - rather than using (mega)parsec, I rolled my own applicative-style parser largely based on cronokirby's [excellent blog post](https://cronokirby.com/posts/2020/12/haskell-in-haskell-3/).
At this point, I also think the hlox implementation is quite a bit _messy_ relative to something I'd be comfortable submitting for code review and running in production.

While hlox is still incomplete (mainly lacking support for classes), I'm delighted that it can handle non-trivial functions and closures, as in

```
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }
  return count;
}
var counter = makeCounter();
counter(); // "1".
counter(); // "2".
```

## Failures and pain points
I came across a pain point when trying to implement `return` statements (which applies equally well to any non-local control flow, like `break`, or `continue`).
There will generally be a Haskell function recursively evaluating each node of the AST, but when a `return` statement is hit, we want to "jump back up the stack" and continue control flow to return from the current _lox_ function.

For example, when evaluating
```
fun foo() {
  var i = 0;
  while (true) {
    if (i == 10) {
      return; // <-- HERE
    }
    i = i + 1;
  }
}
```
at the time hlox evaluates the `return` statement, it needs to exit out of **four** Haskell functions - one each for evaluating the `return`, the `if`, the `while` and the whole lox function call.
In jlox, Bob uses Java exceptions to achieve this non-local control flow.
Haskell _does_ have Java-like exceptions in the IO monad, but I couldn't overcome the feeling that doing such a thing in Haskell would be just so _un-Haskell_.

I ended up stumbling on to some blog posts pointing to _Continuation Passing Style_ as an elegant way to implement complex control flow structures, including the control flow of interpreters.
I spent a fun couple of days reading up on CPS and the associated `Cont` and `ContT` types.
It was especially satisfying to discover that the `Cont` type (and its transformer variant, `ContT`) forms a monad, which makes it nigh trivial to express a CPS program using plain old `do` notation.

I then suspected I would be able to simply sprinkle a `ContT` into the monad transformer stack which makes up the `Lox` type, but I struggled with how to deal with the type parater `r`.
The full type of `ContT` is `ContT r m a`, where `r` is the type of the eventual result.
I first tried adding an extra `r` type parameter to the `Lox` type, but I found myself unable to work through type-checker errors of the form "that r is not the same as this r."

I'd love to revisit the CPS approach at some point, but I was itching to make progress, so I decided to change tack and implement `return` using the `ExceptT` monad.
Despite the name, a value of type `ExceptT` is not related to exceptions in the IO monad, and is more akin to a value of type `Either`.
This feels like an acceptable compromise for now.
While `ExceptT` does not feel as elegant as the CPS approach, it at least feels "pure" in the sense that it doesn't need to rely on any IO monad magic.

I'd still love to return to the CPS approach since it seems both much more efficient and semantically satisfying.
If I do, I'd love to be able to write up the blog post I wish I'd been able to find before running into all the type-checker trouble :)
