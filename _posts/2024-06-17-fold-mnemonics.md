---
layout: post
title: "Mnemonics for Folds"
author: <a href="https://arialdomartini.github.io">Arialdo Martini</a>
tags:
- functional programming
- haskell
- fsharp
---
I've always had trouble remembering the subtle differences between
`FoldLeft` and `FoldRight`, and have constantly found myself
consulting Hoogle over and over.  
Overtime, I collected some mnemonics to help my gold fish memory.  
I hope they can help you too.
<!--more-->

<style>
  .highlighted { 
    color: blue;
    font-weight: bold;
  }
  .left { 
    color: green;
    font-weight: bold;
  }
  .right { 
    color: blue;
    font-weight: bold;
  }
  .left-right td:nth-child(3),   .left-right th:nth-child(3)  {
    text-align: right;
  }
</style>


<table class="left-right">
  <thead>
    <tr>
      <th>&nbsp;</th>
      <th>Fold<span class="left">Left</span><br/><span style="font-size:300%">&nbsp;⇤</span></th>
      <th>Fold<span class="right">Right</span><br/><span style="font-size:300%">⇥&nbsp;</span></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Accumulator</td>
      <td>
          <p><span class="left">Left</span> argument</p>
          <p><tt>(<span class="left">acc</span> -&gt; b -&gt; acc)<br>&nbsp; -&gt; acc<br>&nbsp; -&gt; t b<br>&nbsp; -&gt; acc</tt></p>
      </td>
      <td>
          <p><span class="right">Right</span> argument</p>
          <p><tt>(b -&gt; <span class="right">acc</span> -&gt; acc)<br> -&gt; acc&nbsp;&nbsp;&nbsp;<br> -&gt; t b&nbsp;&nbsp;&nbsp;<br> -&gt; acc&nbsp;&nbsp;&nbsp;</tt></p>
      </td>
    </tr>
    <tr>
      <td>Initial value evaluation</td>
      <td>
          <p>Applied <span class="left">first</span> at the <span class="left">left</span>-hand side of the unrolled expression</p>
          <p>
             <tt>foldl ● initial [a, b, c]<br>(((<span class="left">initial</span> ● a) ● b) ● c)</tt><br/>
          </p>
      </td>
      <td>
          <p>Applied <span class="right">last</span> at the <span class="right">right</span>-hand side of an unrolled expression.</p>
          <p><tt>foldr ● initial [a, b, c]<br>a ● (b ● (c ● <span class="highlighted">initial</span>))</tt></p>
      </td>
    </tr>
    <tr>
      <td>Associativity</td>
      <td>
          <p><span class="left">Left</span></p>
          <p><tt><span class="left">(</span><span class="left">(</span><span class="left">(</span>initial ● a) ● b) ● c)</tt></p>
      </td>
      <td>
          <p><span class="right">Right</span></p>
          <p><tt>(a ● (b ● ( c ● initial<span class="highlighted">)</span><span class="highlighted">)</span><span class="highlighted">)</span></tt></p>
      </td>
    </tr>
    <tr>
      <td>Function application<br>(partial, on a list)</td>
      <td>
        <p>
            <span class="left"><tt>foldl</tt></span> is the outer function.
        </p>
        <p>
            <code class="highlighter-rouge">
                <span class="left">foldl</span> f acc (x:xs) =  <br/>
                &nbsp;&nbsp;&nbsp;&nbsp;<span
                class="left">foldl</span> f (f acc x) xs
            </code>
        </p>
      </td>
      <td>
        <p>
            <span class="right"><tt>f</tt></span> is the outer function.
        </p>
        <p>
           <code class="highlighter-rouge">
               foldr <span class="right">f</span> acc (x:xs) = &nbsp;&nbsp;&nbsp;&nbsp;<br/>
               <span class="right">f</span> x (foldr f acc xs)
          </code>
        </p>
      </td>

    </tr>
  </tbody>
</table>

