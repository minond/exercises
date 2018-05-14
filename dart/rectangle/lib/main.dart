import 'package:flutter/material.dart';

void main() => runApp(MaterialApp(
    debugShowCheckedModeBanner: false,
    title: 'Hello Rectangle',
    home: Scaffold(body: helloRectangle())));

Widget helloRectangle() => Center(
    child: Container(
        color: Colors.greenAccent,
        height: 300.0,
        width: 300.0,
        child: Center(
            child: Text('Hello',
                style: TextStyle(fontSize: 40.0),
                textAlign: TextAlign.center))));
