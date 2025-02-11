[![](https://img.shields.io/badge/enamelese_1.0.0-passing-green)](https://github.com/gongahkia/enamelese/releases/tag/1.0.0) 

# `Enamelese` 🦷

`Enamelese` is a tiny but turing-complete [esolang](https://esolangs.org/wiki/Main_Page) for villagers with a [working *(?)*](https://www.reddit.com/r/answers/comments/6oo3w8/how_would_you_describe_the_phrase_it_only_works/) REPL and Python transpiler. 

`Enamelese` script files have the file extension `.enamel`.

Implemented in [5 hours 31 minutes](https://github.com/gongahkia/enamelese/commit/74c1bf881afa7a80e32c763e1c9cfa174b5c2e90) to scratch an itch I've been having for the past week and a half.

More on my poor decisions [here](#reference).

## Usage

```console
$ git clone https://github.com/gongahkia/enamelese
$ cd src
$ sudo apt install ghc
$ ghc -o enamel Main.hs
```

Then, run any of the following.

```console
$ ./enamel repl
$ ./enamel run <your_file>.enamel
$ ./enamel transpile <your_file>.enamel
```

## Syntax

Syntax is broken down in more detail at [`enamelese.gf`](./enamelese.gf).

| Purpose | Syntax |
| :--- | :---: |
| Program Start | `🏝️` |
| Program End | `🛫` |
| Comment | `🎣 <comment_text>` |
| Variable Declaration | `<villager_name> moves in: <value>` |
| Variable Assignment | `<villager_name> learns: <value>` |
| Print Statement | `<villager_name> says: <value>` |
| Input Statement | `<villager_name> listens:` |
| If Statement | `🤔 <condition>` |
| Else Statement | `🙃` |
| End If | `😌` |
| Loop Start | `🏃‍♂️ <condition>` |
| Loop End | `😴` |
| Function Definition | `🎁 <function_name>(<parameters>)` |
| Function End | `🎀` |
| Function Call | `🔔<function_name>(<arguments>)🔔` |
| Class Definition | `🏠 <class_name>` |
| Class End | `🏠` |
| String Literal | `💬<text>💬` |
| Number Literal | `🔔<number>🔔` |
| Boolean True | `🦉` |
| Boolean False | `🦝` |
| List | `🌴<item1>, <item2>, ...🌴` |
| Dictionary | `🏠<key1>: <value1>, <key2>: <value2>, ...🏠` |
| Addition | `🍎` |
| Subtraction | `🍐` |
| Multiplication | `🍊` |
| Division | `🍑` |
| Modulo | `🥥` |
| Equal To | `🐠` |
| Not Equal To | `🦈` |
| Greater Than | `🐙` |
| Less Than | `🦀` |
| Logical AND | `🦋` |
| Logical OR | `🐝` |
| Logical NOT | `🐞` |
| Try-Catch Start | `🎭` |
| Catch | `🃏 <error_type>` |
| Try-Catch End | `🎭` |

## Examples

`Enamelse` worked on 3 Leetcode questions.

### [1. Two Sum](https://leetcode.com/problems/two-sum/description/)

```txt
🏝️

🎁 two_sum(nums, target)
  seen moves in: 🏠🏠
  i moves in: 🔔0🔔
  
  🏃‍♂️ i 🦀 📏(nums)
    complement moves in: target 🍐 nums[i]
    
    🤔 complement 🐠 seen
      🌴seen[complement], i🌴
    😌
    
    seen[nums[i]] learns: i
    i learns: i 🍎 🔔1🔔
  😴
  
  🌴🌴
🎀

nums moves in: 🌴🔔2🔔, 🔔7🔔, 🔔11🔔, 🔔15🔔🌴
target moves in: 🔔9🔔
result moves in: 🔔two_sum(nums, target)🔔
Tom Nook says: 💬Two Sum Result:💬
Tom Nook says: result

🛫
```

### [5. Longest Palindromic Substring](https://leetcode.com/problems/longest-palindromic-substring/description/)

```
🏝️

🎁 expand_around_center(s, left, right)
  🏃‍♂️ left 🐙 🔔0🔔 🦋 right 🦀 📏(s)
    🤔 s[left] 🐠 s[right]
      left learns: left 🍐 🔔1🔔
      right learns: right 🍎 🔔1🔔
    🙃
      😴
    😌
  😴
  🌴left 🍎 🔔1🔔, right 🍐 left🌴
🎀

🎁 longest_palindrome(s)
  🤔 📏(s) 🦀 🔔1🔔
    s
  😌
  start moves in: 🔔0🔔
  max_length moves in: 🔔1🔔
  
  i moves in: 🔔0🔔
  🏃‍♂️ i 🦀 📏(s)
    odd moves in: 🔔expand_around_center(s, i, i)🔔
    even moves in: 🔔expand_around_center(s, i, i 🍎 🔔1🔔)🔔
    
    length moves in: 🔔max(odd[1] 🍐 odd[0], even[1] 🍐 even[0])🔔
    🤔 length 🐙 max_length
      start learns: i 🍐 (length 🍐 🔔1🔔) 🍑 🔔2🔔
      max_length learns: length
    😌
    
    i learns: i 🍎 🔔1🔔
  😴
  
  s[start 🎲 start 🍎 max_length]
🎀

s moves in: 💬babad💬
result moves in: 🔔longest_palindrome(s)🔔
Tom Nook says: 💬Longest Palindromic Substring:💬
Tom Nook says: result

🛫
```

### [4. Median of Two Sorted Arrays](https://leetcode.com/problems/median-of-two-sorted-arrays/description/)

```
🏝️

🎁 find_kth_element(nums1, nums2, k)
  🤔 📏(nums1) 🐠 🔔0🔔
    nums2[k 🍐 🔔1🔔]
  😌
  🤔 📏(nums2) 🐠 🔔0🔔
    nums1[k 🍐 🔔1🔔]
  😌
  
  i moves in: k 🍑 🔔2🔔
  j moves in: k 🍑 🔔2🔔
  
  🏃‍♂️ 🦉
    🤔 i 🐙 📏(nums1)
      nums2[j 🍐 🔔1🔔]
    😌
    🤔 j 🐙 📏(nums2)
      nums1[i 🍐 🔔1🔔]
    😌
    
    🤔 nums1[i] 🦀 nums2[j]
      🤔 i 🍎 🔔1🔔 🐠 k
        nums1[i]
      😌
      i learns: i 🍎 🔔1🔔
      k learns: k 🍐 🔔1🔔
    🙃
      🤔 j 🍎 🔔1🔔 🐠 k
        nums2[j]
      😌
      j learns: j 🍎 🔔1🔔
      k learns: k 🍐 🔔1🔔
    😌
  😴
🎀

🎁 find_median_sorted_arrays(nums1, nums2)
  m moves in: 📏(nums1)
  n moves in: 📏(nums2)
  total moves in: m 🍎 n
  
  🤔 total 🥥 🔔2🔔 🐠 🔔0🔔
    k moves in: total 🍑 🔔2🔔
    🔔find_kth_element(nums1, nums2, k)🔔
  🙃
    k1 moves in: total 🍑 🔔2🔔
    k2 moves in: k1 🍎 🔔1🔔
    (🔔find_kth_element(nums1, nums2, k1)🔔 🍎 🔔find_kth_element(nums1, nums2, k2)🔔) 🍑 🔔2.0🔔
  😌
🎀

nums1 moves in: 🌴🔔1🔔, 🔔3🔔🌴
nums2 moves in: 🌴🔔2🔔🌴
result moves in: 🔔find_median_sorted_arrays(nums1, nums2)🔔
Tom Nook says: 💬Median of Two Sorted Arrays:💬
Tom Nook says: result

🛫
```

## Reference

The name `Enamelese` is in reference to [Animalese](https://nookipedia.com/wiki/Animalese), the language spoken by most inhabitants in the [*Animal Crossing*](https://nookipedia.com/wiki/Animal_Crossing_(series)) series. As for the [double entendre](https://dictionary.cambridge.org/dictionary/english/double-entendre) involving the word [enamel](https://dictionary.cambridge.org/dictionary/english/enamel), that's because [K.K Slider](https://animalcrossing.fandom.com/wiki/K.K._Slider)'s teeth scare me.

![](./asset/my-hero.jpg)

## Other interesting projects that involve Animalese

* [animalese.js](https://github.com/Acedio/animalese.js) by *Acedio*
* [animalese.py](https://github.com/DigiDuncan/animalese.py) by *DigiDuncan*

## The esolang community will beat my ass if I don't mention it here

* [Emoji](https://esolangs.org/wiki/Emoji) esolang
