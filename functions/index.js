const functions = require("firebase-functions");
const admin = require("firebase-admin");
admin.initializeApp();

const db = admin.firestore();

const HAS_REMINDER_ON_KEY = "has_reminder_on";
const REMINDER_TIME_HOUR_KEY = "reminder_time_hour";
const REMINDER_TIME_MINUTE_KEY = "reminder_time_minute";
const EQUALS = "==";

exports.sendScheduledReminders = functions.pubsub
    .schedule("every 30 minutes synchronized").onRun(sendReminders);

async function sendReminders(context) {
  const today = new Date();
  const currentHour = today.getUTCHours();
  const currentMinute = roundToHalfHour(today.getUTCMinutes());
  console.log({currentHour, currentMinute});
  const snapshot = await db.collection("users")
      .where(HAS_REMINDER_ON_KEY, EQUALS, true)
      .where(REMINDER_TIME_HOUR_KEY, EQUALS, currentHour)
      .where(REMINDER_TIME_MINUTE_KEY, EQUALS, currentMinute)
      .get();

  if (snapshot.empty) {
    console.log("No users to notify");
    return;
  }

  snapshot.forEach((doc) => {
    console.log(doc.id);
  });
}

function roundToHalfHour(minutes) {
  if (minutes % 30 === 0) {
    return minutes;
  } else if (minutes < 15) {
    return 0;
  }
  return 30;
}
