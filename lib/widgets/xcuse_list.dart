import 'package:flutter/material.dart';
import 'package:xcuseme/constants/style.dart';
import 'package:xcuseme/widgets/event_tile.dart';
import 'package:xcuseme/model.dart';
import 'package:provider/provider.dart';
import 'package:xcuseme/models/event.dart';

class XCuseList extends StatelessWidget {
  final Model model;

  XCuseList(this.model);

  Widget build(BuildContext context) {
    List<Event> events = context.watch<List<Event>>();
    if (events.isEmpty) {
      return Text("Nothing logged yet...",
          style: TextStyle(
              fontSize: SMALL_HEADING_FONT_SIZE, fontStyle: FontStyle.italic));
    }
    events.sort((a, b) => b.datetime.compareTo(a.datetime));

    return Expanded(
        child: ListView.separated(
      itemCount: events.length,
      itemBuilder: (BuildContext context, int index) {
        Event e = events.elementAt(index);
        return EventTile(e);
      },
      separatorBuilder: (BuildContext context, int index) => const Divider(),
    ));
  }
}
