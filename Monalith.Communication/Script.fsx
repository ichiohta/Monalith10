#load "Monalith.Communication.fs"
open Monalith.Communication
open System
open System.Text.RegularExpressions

Parser.parsePosts
  [| "なまえをいれてください<>sage<>2014/07/01(火) 21:28:34.42 ID:wcKyb1is<> 前スレ <br> 積みゲーを語るスレ　35段目 <br> http://mastiff.2ch.net/test/read.cgi/famicom/1370397200/ <br>  <br> http://www11.atwiki.jp/tumige/ <br>  <br> 関連スレ <br> 積みゲーを崩すスレPt6 <br> http://mastiff.2ch.net/test/read.cgi/famicom/1353078404/ <>積みゲーを語るスレ　36段目" |]
  |> Seq.last

Parser.parseThreads (Uri "http://some.2ch.net")
  [| "1415069148.dat<>どうして日本は2番目のabe大統領が皆決めることなのか [転載禁止]&copy;2ch.net	 (4)" |]
  |> Seq.last

let categories =
    HttpClient.getDefaultCategoriesAsync 
    |> Async.RunSynchronously
    |> Seq.toArray

let category =
    categories
    |> Seq.filter(fun c -> c.Name.Equals("案内"))
    |> Seq.last

let board =
    category.Boards
    |> Seq.filter(fun b -> b.Name.Equals("ラウンジ"))
    |> Seq.last

let threads =
    HttpClient.getThreadsAsync board
    |> Async.RunSynchronously
    |> Seq.toArray

let thread =
    threads
    |> Seq.filter(fun t -> t.Name.Equals("韓国関連で一番ムカついたこと"))
    |> Seq.last

let posts =
    HttpClient.getPostsAsync thread
    |> Async.RunSynchronously
    |> Seq.toArray
