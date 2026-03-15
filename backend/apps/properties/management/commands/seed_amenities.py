from django.core.management.base import BaseCommand

from apps.properties.models import Amenity


DEFAULT_AMENITIES = [
    "WiFi",
    "Dedicated workspace",
    "Air conditioning",
    "Heating",
    "Ceiling fan",
    "TV",
    "Smart TV",
    "Cable TV",
    "Netflix",
    "Bluetooth speaker",
    "Board games",
    "Books and reading material",
    "Kitchen",
    "Kitchenette",
    "Refrigerator",
    "Microwave",
    "Oven",
    "Stove",
    "Dishwasher",
    "Coffee maker",
    "Kettle",
    "Toaster",
    "Blender",
    "Rice cooker",
    "Dining table",
    "Cookware",
    "Dishes and silverware",
    "Wine glasses",
    "Hot water",
    "Washer",
    "Dryer",
    "Iron",
    "Ironing board",
    "Clothes storage",
    "Hangers",
    "Bed linens",
    "Extra pillows and blankets",
    "Hair dryer",
    "Shampoo",
    "Conditioner",
    "Body soap",
    "Shower gel",
    "Towels",
    "First aid kit",
    "Smoke alarm",
    "Carbon monoxide alarm",
    "Fire extinguisher",
    "Security cameras",
    "Safe",
    "Lockbox",
    "Private entrance",
    "Self check-in",
    "Keypad",
    "Doorman",
    "Elevator",
    "Wheelchair accessible",
    "Step-free access",
    "Wide doorway",
    "Accessible bathroom",
    "Grab rails",
    "Roll-in shower",
    "Outdoor shower",
    "Pool",
    "Private pool",
    "Hot tub",
    "Sauna",
    "Gym",
    "Yoga mat",
    "Massage table",
    "Garden",
    "Patio",
    "Balcony",
    "Terrace",
    "Outdoor dining area",
    "BBQ grill",
    "Fire pit",
    "Sun loungers",
    "Beach access",
    "Lake access",
    "River access",
    "Mountain view",
    "Sea view",
    "City skyline view",
    "Free parking",
    "Paid parking",
    "Street parking",
    "EV charger",
    "Luggage drop-off",
    "24/7 check-in",
    "Long-term stays allowed",
    "Pets allowed",
    "Pet bowls",
    "Crib",
    "Travel crib",
    "High chair",
    "Children's books and toys",
    "Babysitter recommendations",
    "Breakfast included",
    "Room service",
    "Daily housekeeping",
    "Airport shuttle",
    "Car rental",
    "Bicycle rental",
]


class Command(BaseCommand):
    help = "Create a broad default list of amenities (idempotent)."

    def add_arguments(self, parser):
        parser.add_argument(
            "--clear",
            action="store_true",
            help="Delete existing amenities before inserting defaults.",
        )

    def handle(self, *args, **options):
        if options["clear"]:
            deleted_count, _ = Amenity.objects.all().delete()
            self.stdout.write(self.style.WARNING(f"Deleted {deleted_count} existing amenity row(s)."))

        created_count = 0
        existing_count = 0

        for amenity_name in DEFAULT_AMENITIES:
            _, created = Amenity.objects.get_or_create(name=amenity_name)
            if created:
                created_count += 1
            else:
                existing_count += 1

        self.stdout.write(self.style.SUCCESS(f"Created: {created_count}"))
        self.stdout.write(self.style.NOTICE(f"Already existed: {existing_count}"))
        self.stdout.write(self.style.SUCCESS("Amenities seed completed."))
