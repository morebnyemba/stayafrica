# Messaging System Improvements

## Overview
Enhanced the messaging system with conversation threading, message types, templates, and advanced features suitable for a property rental platform.

## New Models

### 1. Conversation Model
**Purpose**: Thread messages between users with context about properties or bookings

**Key Features**:
- ManyToMany participants (supports 2+ users in a conversation)
- Optional property/booking foreign keys for context
- Subject field for conversation topics
- Archived_by ManyToMany for per-user archiving
- Automatic updated_at on message creation

**Methods**:
- `get_last_message()`: Returns most recent message
- `get_unread_count(user)`: Returns unread count for specific user
- `mark_as_read(user)`: Marks all messages as read for user

**Example Usage**:
```python
# Create conversation about a property
conversation = Conversation.objects.create(
    property_id=property_id,
    subject="Inquiry about Beach House"
)
conversation.participants.set([guest, host])

# Get unread count
unread = conversation.get_unread_count(guest)

# Mark all as read
conversation.mark_as_read(guest)
```

### 2. Enhanced Message Model
**New Fields**:
- `conversation`: ForeignKey to Conversation (threading)
- `message_type`: CharField with choices (text, system, booking_request, booking_confirmation, booking_cancellation, review_reminder, payment_confirmation)
- `metadata`: JSONField for attachments, links, and additional data
- `read_at`: DateTimeField (timestamp when message was read)
- `edited_at`: DateTimeField (tracks when message was edited)
- `deleted_by_sender`: DateTimeField (soft delete for sender)
- `deleted_by_receiver`: DateTimeField (soft delete for receiver)

**Message Types**:
- `text`: Regular user messages
- `system`: Automated system notifications
- `booking_request`: When guest requests booking
- `booking_confirmation`: When booking is confirmed
- `booking_cancellation`: When booking is cancelled
- `review_reminder`: Reminder to leave review
- `payment_confirmation`: Payment successful notifications

**Metadata Examples**:
```json
{
  "attachments": ["https://example.com/image.jpg"],
  "booking_id": "1234567890",
  "property_title": "Beach House",
  "check_in": "2024-01-15",
  "check_out": "2024-01-20"
}
```

### 3. MessageTemplate Model
**Purpose**: Reusable message templates with variable substitution

**Features**:
- Template types matching message types
- Subject and body with {variable} placeholders
- `render(variables)` method for variable substitution
- Active/inactive flag for template management

**Example Usage**:
```python
template = MessageTemplate.objects.create(
    name="Booking Confirmation",
    template_type="booking_confirmation",
    subject="Booking Confirmed - {property_title}",
    body="Your booking for {property_title} from {check_in} to {check_out} has been confirmed!"
)

# Render with variables
rendered = template.render({
    'property_title': 'Beach House',
    'check_in': '2024-01-15',
    'check_out': '2024-01-20'
})
```

## New Serializers

### ConversationSerializer
- Lists conversations with last message preview
- Shows other participant (not current user)
- Includes unread count per conversation
- Property/booking context included

### ConversationDetailSerializer
- Extends ConversationSerializer
- Includes all messages in conversation
- Used for conversation detail view

### MessageCreateSerializer
- Validates sender/receiver are conversation participants
- Enforces conversation context

### MessageTemplateSerializer
- Exposes template fields for frontend
- Used by MessageTemplateViewSet

## New API Endpoints

### ConversationViewSet

#### List Conversations
```
GET /api/messaging/conversations/
```
Returns conversations for current user with last message and unread count.

Query Parameters:
- `archived=true`: Include archived conversations

#### Create Conversation
```
POST /api/messaging/conversations/
{
  "participants": [user_id_1, user_id_2],
  "property": property_id,
  "booking": booking_id,
  "subject": "Inquiry about property"
}
```
Creates new conversation or returns existing one if found.

#### Retrieve Conversation
```
GET /api/messaging/conversations/{id}/
```
Returns conversation with all messages.

#### Mark as Read
```
POST /api/messaging/conversations/{id}/mark_as_read/
```
Marks all messages in conversation as read for current user.

#### Archive/Unarchive
```
POST /api/messaging/conversations/{id}/archive/
```
Toggles archive status for current user.

#### Total Unread Count
```
GET /api/messaging/conversations/unread_count/
```
Returns total unread messages across all conversations.

### MessageViewSet (Enhanced)

#### List Messages
```
GET /api/messaging/messages/
```
Query Parameters:
- `conversation={id}`: Filter by conversation
- `message_type={type}`: Filter by message type
- `unread=true`: Only unread messages

#### Create Message
```
POST /api/messaging/messages/
{
  "conversation": conversation_id,
  "receiver": user_id,
  "text": "Message content",
  "message_type": "text",
  "metadata": {"key": "value"}
}
```

#### Edit Message
```
PUT /api/messaging/messages/{id}/edit/
{
  "text": "Updated message content"
}
```
Only message sender can edit. Sets edited_at timestamp.

#### Delete Message (Soft Delete)
```
DELETE /api/messaging/messages/{id}/
```
Soft deletes message (sets deleted_by_sender or deleted_by_receiver).

#### Mark as Read
```
POST /api/messaging/messages/{id}/mark_as_read/
```
Marks single message as read with timestamp.

### MessageTemplateViewSet

#### List Templates
```
GET /api/messaging/templates/
```
Returns all active templates.

#### Render Template
```
POST /api/messaging/templates/{id}/render/
{
  "variables": {
    "property_title": "Beach House",
    "guest_name": "John Doe"
  }
}
```
Renders template with provided variables.

## Key Features

### 1. Conversation Threading
- Messages grouped into conversations
- Conversations can reference properties or bookings
- Multi-user support (not just 1-on-1)
- Per-user archiving

### 2. Message Types
- System can send automated messages
- Different types for different scenarios
- Frontend can style differently based on type

### 3. Metadata Support
- Attach rich data to messages (booking details, property info)
- Store links, attachments, structured data
- Extensible for future needs

### 4. Read Receipts
- Track when messages are read
- Timestamp for read status
- Per-conversation unread counts

### 5. Soft Delete
- Users can delete messages from their view
- Other participant still sees message
- Maintains conversation integrity

### 6. Message Editing
- Users can edit their own messages
- Tracks edit timestamp
- Edit history could be added later

### 7. Template System
- Reusable message templates
- Variable substitution
- Consistency in automated messages

## Use Cases

### Property Inquiry
```python
# Guest inquires about property
conversation = Conversation.objects.create(
    property=property,
    subject=f"Inquiry about {property.title}"
)
conversation.participants.set([guest, host])

message = Message.objects.create(
    conversation=conversation,
    sender=guest,
    receiver=host,
    text="Is this property available for Feb 1-5?",
    message_type="text"
)
```

### Booking Request
```python
# System creates booking request message
message = Message.objects.create(
    conversation=conversation,
    sender=system_user,
    receiver=host,
    text="New booking request for your property",
    message_type="booking_request",
    metadata={
        "booking_id": booking.id,
        "check_in": "2024-02-01",
        "check_out": "2024-02-05",
        "guests": 4
    }
)
```

### Booking Confirmation
```python
# Use template for confirmation
template = MessageTemplate.objects.get(template_type="booking_confirmation")
rendered = template.render({
    'property_title': property.title,
    'check_in': booking.check_in,
    'check_out': booking.check_out
})

message = Message.objects.create(
    conversation=conversation,
    sender=system_user,
    receiver=guest,
    text=rendered['body'],
    message_type="booking_confirmation",
    metadata={'booking_id': booking.id}
)
```

### Review Reminder
```python
# Send automated review reminder 3 days after checkout
message = Message.objects.create(
    conversation=conversation,
    sender=system_user,
    receiver=guest,
    text="How was your stay? Leave a review!",
    message_type="review_reminder",
    metadata={
        'booking_id': booking.id,
        'property_id': property.id
    }
)
```

## Migration Steps

1. **Create Migration**:
   ```bash
   docker compose -f docker-compose.prod.yml exec backend python manage.py makemigrations messaging --name enhance_messaging_with_conversations
   ```

2. **Review Migration**:
   Check generated migration file for correctness

3. **Apply Migration**:
   ```bash
   docker compose -f docker-compose.prod.yml exec backend python manage.py migrate messaging
   ```

4. **Create Sample Templates** (optional):
   ```python
   # In Django shell or management command
   MessageTemplate.objects.create(
       name="Booking Confirmation",
       template_type="booking_confirmation",
       subject="Booking Confirmed - {property_title}",
       body="Your booking for {property_title} from {check_in} to {check_out} has been confirmed! Total: {total_price}",
       is_active=True
   )
   ```

## Data Migration (Existing Messages)

If you have existing messages, you'll need to migrate them to conversations:

```python
# In a data migration or management command
from apps.messaging.models import Message, Conversation

for message in Message.objects.filter(conversation__isnull=True):
    # Create conversation between sender and receiver
    conv = Conversation.objects.filter(
        participants__in=[message.sender, message.receiver]
    ).filter(
        property__isnull=True,
        booking__isnull=True
    ).first()
    
    if not conv:
        conv = Conversation.objects.create()
        conv.participants.set([message.sender, message.receiver])
    
    message.conversation = conv
    message.save()
```

## Frontend Integration Notes

### Conversation List UI
- Show last message preview
- Display unread badge
- Show other participant name/avatar
- Property/booking context if available

### Conversation Detail UI
- Thread messages by conversation
- Group messages by date
- Show read/unread status
- Display edit indicator if message was edited
- Different styling for message types (system messages, booking confirmations, etc.)

### Message Composition
- Allow selecting message type (usually "text" for user messages)
- Support metadata for attachments
- Real-time updates via WebSocket (future enhancement)

### Notifications
- Push notification on new message
- Badge count from `/conversations/unread_count/`
- Different notification types based on message_type

## Future Enhancements

1. **Real-time Messaging**: WebSocket support for instant message delivery
2. **Typing Indicators**: Show when other user is typing
3. **File Attachments**: Direct file upload support with metadata
4. **Message Reactions**: Like/emoji reactions to messages
5. **Message Search**: Full-text search across conversations
6. **Group Conversations**: Multi-user conversations (already supported in model)
7. **Message History**: Track all edits, not just most recent
8. **Scheduled Messages**: Send messages at specific time
9. **Auto-responses**: Automated replies based on triggers

## Testing

### Unit Tests
```python
def test_conversation_unread_count():
    conv = Conversation.objects.create()
    conv.participants.set([user1, user2])
    
    # User2 sends 3 messages to user1
    for i in range(3):
        Message.objects.create(
            conversation=conv,
            sender=user2,
            receiver=user1,
            text=f"Message {i}"
        )
    
    assert conv.get_unread_count(user1) == 3
    assert conv.get_unread_count(user2) == 0
```

### Integration Tests
```python
def test_create_conversation_api(api_client):
    response = api_client.post('/api/messaging/conversations/', {
        'participants': [user1.id, user2.id],
        'property': property.id,
        'subject': 'Test conversation'
    })
    
    assert response.status_code == 201
    assert response.data['property'] == property.id
```

## Performance Considerations

1. **Database Indexes**: Add indexes on conversation participants, message conversation FK
2. **Pagination**: Use DRF pagination for message lists (already implemented)
3. **Query Optimization**: Use select_related and prefetch_related (already implemented)
4. **Caching**: Cache unread counts, conversation lists for frequently accessed data
5. **Bulk Operations**: Use bulk_create for multiple messages

## Security Notes

- Users can only access conversations they're participants in
- Users can only mark their own messages as read
- Users can only edit/delete their own messages
- Soft deletes prevent data loss while respecting user privacy
- Conversation participants are validated on message creation
