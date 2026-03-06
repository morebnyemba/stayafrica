cd "D:\Projects\stayafrica-1"

git add "mobile/app/_layout.tsx" "mobile/app/(tabs)/explore/[id].tsx"
Write-Host "git add completed"

git commit -m @"
fix(mobile): prevent splash/welcome flash and add POI directions button

- _layout.tsx: Navigate BEFORE removing branded splash overlay. The 120ms
  delay lets the router settle on the correct screen, eliminating the
  flash of the default Stack route between splash dismissal and navigation.
- [id].tsx: Add 'Go' directions button to each POI item that opens native
  maps (Apple Maps on iOS, Google Maps on Android) with lat/lng coords.
  Falls back to Google Maps web URL if native maps unavailable.

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>
"@

Write-Host "git commit completed"
