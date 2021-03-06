import 'package:firebase_core/firebase_core.dart';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:firebase_messaging/firebase_messaging.dart';
import 'package:flutter/services.dart';
import 'package:flutter/material.dart';
import 'package:xcuseme/model.dart';
import 'package:xcuseme/notifications.dart';
import 'package:xcuseme/pages/create.dart';
import 'package:xcuseme/pages/edit.dart';
import 'package:xcuseme/pages/details.dart';
import 'package:xcuseme/pages/info.dart';
import 'package:xcuseme/pages/loading.dart';
import 'package:xcuseme/pages/login.dart';
import 'package:xcuseme/pages/home.dart';
import 'package:xcuseme/pages/settings.dart';
import 'package:xcuseme/widgets/drawer.dart';
import 'package:xcuseme/models/event.dart';
import 'package:xcuseme/authentication_service.dart';
import 'package:xcuseme/firestore_service.dart';
import 'package:provider/provider.dart';
import 'dart:async';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  SystemChrome.setPreferredOrientations([
    DeviceOrientation.portraitUp,
  ]);
  createNotificationChannel();
  runApp(
    ChangeNotifierProvider<Model>(
      create: (context) => Model(),
      child: InitializationWrapper(),
    ),
  );
}

class XCuseMeApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'XCuseMe',
      initialRoute: '/',
      routes: {
        '/': (context) => AuthProvider(
            child: HomePage(context.watch<Model>()), currentRoute: '/'),
        '/log-excuse': (context) => AuthProvider(
            child: CreatePage(EventType.EXCUSE), currentRoute: '/log-excuse'),
        '/log-exercise': (context) => AuthProvider(
            child: CreatePage(EventType.EXERCISE),
            currentRoute: '/log-exercise'),
        '/info': (context) =>
            AuthProvider(child: InfoPage(), currentRoute: '/info'),
        '/details': (context) =>
            AuthProvider(child: DetailsPage(), currentRoute: '/details'),
        '/edit': (context) =>
            AuthProvider(child: EditPage(), currentRoute: '/edit'),
        '/settings': (context) => AuthProvider(
            child: SettingsStreamProvider(), currentRoute: '/settings'),
      },
    );
  }
}

Future<FirebaseApp> initializeFirebase() async {
  FirebaseApp _firebaseApp = await Firebase.initializeApp();
  String token = await FirebaseMessaging.instance.getToken();
  await FirestoreService().updateTokens(token);
  FirebaseMessaging.instance.onTokenRefresh
      .listen((token) => FirestoreService().updateTokens(token));
  return _firebaseApp;
}

class InitializationWrapper extends StatelessWidget {
  final Future<FirebaseApp> _firebaseApp = initializeFirebase();

  @override
  Widget build(BuildContext context) {
    return FutureBuilder<FirebaseApp>(
      future: _firebaseApp,
      builder: (BuildContext context, AsyncSnapshot<FirebaseApp> snapshot) {
        if (snapshot.hasData) {
          return XCuseMeApp();
        } else if (snapshot.hasError) {
          return Material(child: Text('uh oh'));
        } else {
          return MaterialApp(home: LoadingPage());
        }
      },
    );
  }
}

class AuthProvider extends StatelessWidget {
  final Widget child;
  final String currentRoute;

  AuthProvider({this.child, this.currentRoute});

  @override
  Widget build(BuildContext context) {
    return MultiProvider(
      providers: [
        Provider<AuthenticationService>(
          create: (_) => AuthenticationService(FirebaseAuth.instance),
        ),
        StreamProvider(
          create: (context) =>
              context.read<AuthenticationService>().authStateChanges,
          initialData: null,
        ),
      ],
      child: AuthWrapper(child: child, currentRoute: currentRoute),
    );
  }
}

class EventStreamProvider extends StatelessWidget {
  final Widget child;

  EventStreamProvider(this.child);

  @override
  Widget build(BuildContext context) {
    return StreamProvider<List<Event>>(
        create: (context) =>
            FirestoreService().eventStream(user: context.read<User>()),
        initialData: [],
        child: child);
  }
}

class AuthWrapper extends StatelessWidget {
  final Widget child;
  final String currentRoute;

  AuthWrapper({this.child, this.currentRoute});

  @override
  Widget build(BuildContext context) {
    final firebaseUser = context.watch<User>();

    if (firebaseUser != null) {
      return XCuseMeScaffold(body: child, currentRoute: currentRoute);
    } else {
      return LoginPage();
    }
  }
}

class XCuseMeScaffold extends StatelessWidget {
  final Widget body;
  final String currentRoute;

  XCuseMeScaffold({this.body, this.currentRoute});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
            title: Text('XCuseMe', style: TextStyle(color: Colors.white)),
            centerTitle: true,
            flexibleSpace: Container(
                decoration: BoxDecoration(
              color: Colors.indigo[100],
            ))),
        body: EventStreamProvider(this.body),
        drawer: XCuseMeDrawer(currentRoute: currentRoute));
  }
}
