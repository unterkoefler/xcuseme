import 'package:flutter/material.dart';
import 'package:xcuseme/constants/style.dart';
import 'package:xcuseme/constants/constants.dart';
import 'package:xcuseme/widgets/event_tile.dart';
import 'package:xcuseme/models/event.dart';
import 'package:table_calendar/table_calendar.dart';
import 'package:xcuseme/widgets/create_event_tile.dart';
import 'package:xcuseme/model.dart';
import 'package:provider/provider.dart';

class XCuseCalendar extends StatelessWidget {
  final Model model;

  XCuseCalendar(this.model);

  Map<DateTime, List<Event>> _eventsForCal(List<Event> events) {
    Map<DateTime, List<Event>> evs = Map();
    events.forEach((event) {
      evs[event.datetime] = [event];
    });
    return evs;
  }

  Widget _buildEventMarker(DateTime dt, Event event) {
    Color color = TYPE_COLORS[event.type];
    return SizedBox.expand(
      child: Container(
        margin: const EdgeInsets.all(6.0),
        decoration: BoxDecoration(
          shape: BoxShape.circle,
          color: color.withAlpha(80),
        ),
      ),
    );
  }

  Widget _calendar(BuildContext context, List<Event> events) {
    return TableCalendar(
      calendarController: model.calendarController,
      initialSelectedDay: model.selectedDay ?? DateTime.now(),
      endDay: DateTime.now(),
      events: _eventsForCal(events),
      calendarStyle: CalendarStyle(
        selectedColor: Colors.blue[800],
        todayColor: Colors.blue[200],
        weekendStyle: TextStyle(color: Colors.black),
        outsideWeekendStyle: TextStyle(color: Colors.grey[500]),
        contentPadding: EdgeInsets.symmetric(horizontal: 8.0),
        cellMargin: EdgeInsets.all(3.0),
      ),
      daysOfWeekStyle:
          DaysOfWeekStyle(weekendStyle: TextStyle(color: Colors.grey[700])),
      onDaySelected: _onDaySelected,
      availableCalendarFormats: const {
        CalendarFormat.month: '',
      },
      headerStyle: HeaderStyle(
        centerHeaderTitle: true,
        decoration: BoxDecoration(
          color: Colors.indigo[100],
        ),
        titleTextStyle:
            TextStyle(color: Colors.white, fontSize: SMALL_HEADING_FONT_SIZE),
        headerMargin: EdgeInsets.only(bottom: 12.0),
        headerPadding: EdgeInsets.symmetric(vertical: 0.0),
      ),
      builders:
          CalendarBuilders(markersBuilder: (context, date, events, holidays) {
        if (events.isNotEmpty &&
            !model.calendarController.isSelected(date) &&
            !model.calendarController.isToday(date)) {
          return <Widget>[_buildEventMarker(date, events[0])];
        } else {
          return <Widget>[];
        }
      }),
    );
  }

  Widget _eventForSelectedDay(List<Event> events) {
    Event event = events.firstWhere(
        (ev) => isSameDay(ev.datetime, model.selectedDay),
        orElse: () => null);
    if (event == null) {
      return CreateEventTile();
    } else {
      return EventTile(event);
    }
  }

  void _onDaySelected(DateTime day, List events, List holidays) {
    model.updateSelectedDay(day);
  }

  @override
  Widget build(BuildContext context) {
    List<Event> events = context.watch<List<Event>>();
    return Expanded(
        child: Column(
      children: <Widget>[
        Expanded(
            child: SingleChildScrollView(child: _calendar(context, events))),
        const Divider(),
        _eventForSelectedDay(events),
        const Divider()
      ],
    ));
  }
}
