import 'package:flutter/material.dart';

final String title = 'Converter';
final List<ConvertionGroup> groups = <ConvertionGroup>[
  new ConvertionGroup('Length', Icons.filter_1),
  new ConvertionGroup('Area', Icons.filter_2),
  new ConvertionGroup('Volume', Icons.filter_3),
  new ConvertionGroup('Mass', Icons.filter_4),
  new ConvertionGroup('Time', Icons.filter_5),
  new ConvertionGroup('Digital Storage', Icons.filter_6),
  new ConvertionGroup('Energy', Icons.filter_7),
  new ConvertionGroup('Currency', Icons.filter_8),
];

class ConvertionGroup {
  String title;
  IconData icon;
  ConvertionGroup(this.title, this.icon);
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
        onTap: () {
          print('Tapped on ${group.title}');
        },
        child: ListTile(
          leading: Icon(
            group.icon,
            size: 25.0,
            color: Colors.blue,
          ),
          subtitle: Text(
            '${group.title} convertertions',
            style: TextStyle(fontSize: 10.0),
          ),
          title: Text(
            group.title,
            style: TextStyle(fontSize: 20.0),
          ),
        ),
      ),
    );
