import 'package:flutter/material.dart';

final String title = 'Converter';
final List<ConvertionGroup> groups = <ConvertionGroup>[
  new ConvertionGroup('Length'),
  new ConvertionGroup('Area'),
];

class ConvertionGroup {
  String title, logo;
  ConvertionGroup(this.title,
      [this.logo = 'http://cdn.onlinewebfonts.com/svg/img_534751.png']);
}

void main() => runApp(MaterialApp(
    debugShowCheckedModeBanner: false,
    title: title,
    home: Scaffold(body: groupsSelector(groups))));

Widget groupsSelector(List<ConvertionGroup> groups) => ListView.builder(
      itemCount: groups.length,
      itemBuilder: (BuildContext ctx, int i) => groupItem(groups[i]),
    );

Widget groupItem(ConvertionGroup group) => Container(
      child: InkWell(
        child: Row(
          children: <Widget>[
            Padding(
              padding: EdgeInsets.all(10.0),
              child: Text('group.logo'),
              // child: NetworkImage(group.logo),
            ),
            Center(child: Text(group.title)),
          ],
        ),
      ),
    );
