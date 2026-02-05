# stapeln UX Manifesto: "Containers for People Who Hate Containers"

## Core Principle

**"If you have to read the manual, we failed."**

## Design Philosophy

### 1. Visual > Textual
- Show, don't tell
- Icons > words
- Colors > status codes
- Animations > descriptions

### 2. Conversational > Technical
```
❌ Bad:  "Error: EADDRINUSE 0.0.0.0:8080"
✅ Good: "Port 8080 is already taken. Try 8081 instead? [Yes] [Choose another]"
```

### 3. Forgiving > Strict
- Undo button always available
- Auto-save every 30 seconds
- Can't permanently break anything
- Rollback is one click

### 4. Guided > Self-service
- Interactive tour on first launch
- Contextual tooltips everywhere
- Suggest next steps
- "You might want to..."

### 5. Fast > Perfect
- Show progress immediately
- Background processing
- Optimistic UI updates
- Never block the user

## UX Rules

### Rule 1: "2-Second Rule"
**No operation takes longer than 2 seconds without feedback**

Examples:
- Pulling image → Show progress bar
- Building container → Stream logs
- Deploying stack → Animate components

### Rule 2: "Zero Surprise Rule"
**Users should never wonder "what just happened?"**

Examples:
- Click Deploy → Show "Deploying..." animation
- Deployment succeeds → Green checkmark + sound
- Deployment fails → Red X + specific error + fix

### Rule 3: "Grandmother Test"
**If your grandmother can't figure it out, it's too complex**

Examples:
- "Drag this to that" ✅
- "Configure the ingress controller" ❌

### Rule 4: "One-Click Fix Rule"
**Every error message has a [Fix It] button**

Examples:
```
❌ Port conflict
[Use next available port]

❌ Missing health check
[Add default health check]

❌ No resource limits
[Set recommended limits]
```

### Rule 5: "Show, Don't Ask Rule"
**Show what will happen, don't ask for confirmation**

```
❌ Bad:
"Are you sure you want to deploy?"
[Yes] [No]

✅ Good:
[Deploy] → Shows preview → [Looks good!] [Cancel]
```

## Error Message Formula

```
[Icon] [What went wrong in plain English]

[Why it happened - 1 sentence]

[What you can do about it - 3 options max]

[Primary Action Button] [Secondary] [Learn More]
```

**Example**:
```
❌ Container Won't Start

The nginx container can't bind to port 80 because
that port is already in use.

You can:
• Use a different port (recommended: 8080)
• Stop the program using port 80
• See which program is using it

[Use Port 8080] [Show Me] [Cancel]
```

## Visual Hierarchy

### Level 1: Status (immediate understanding)
- ✅ Green = Working
- ❌ Red = Broken
- ⚠️ Yellow = Warning
- 🔵 Blue = Info
- ⚪ Gray = Inactive

### Level 2: Icons (quick recognition)
- 🏔️ Cerro Torre (build)
- 🛡️ Svalinn (gateway)
- 🌉 selur (bridge)
- ⚔️ Vörðr (runtime)
- 🐳 Container
- 📦 Package
- 🔒 Security

### Level 3: Text (detailed information)
- Only when necessary
- Short sentences (<10 words)
- Active voice
- No jargon

## Interaction Patterns

### Pattern 1: Drag-and-Drop
```
1. Hover over component → Tooltip appears
2. Start drag → Component follows cursor
3. Hover over canvas → Drop zone highlights
4. Drop → Component placed + auto-configured
5. Success feedback → Brief pulse animation
```

### Pattern 2: Click-to-Configure
```
1. Click component → Configuration panel slides in from right
2. Change settings → Live preview updates
3. Click Apply → Changes saved
4. Panel slides out
```

### Pattern 3: Connect-by-Line
```
1. Click component A → It highlights
2. Move cursor toward component B → Line draws
3. Click component B → Connection created
4. Auto-configure: environment variables, network, etc.
```

### Pattern 4: Simulation Mode
```
1. Click [Simulate] → Canvas enters simulation mode
2. Animated packets flow between components
3. Green checkmarks appear for success
4. Red X appears if something would fail
5. Click [Exit Simulation] → Back to edit mode
```

## Accessibility First

### Keyboard Navigation
- Tab through all elements
- Enter/Space to activate
- Escape to cancel
- Ctrl+Z to undo
- All operations possible without mouse

### Screen Reader
- Every element has aria-label
- Status changes announced
- Operations described in plain English
- "nginx container selected. Press Enter to configure."

### High Contrast Mode
- Automatic detection
- Manual toggle
- 7:1 contrast ratio minimum
- Works in dark and light modes

### Reduced Motion
- Respects prefers-reduced-motion
- Animations can be disabled
- Crossfade instead of slide
- Instant feedback instead of progress bars

## Performance Targets

| Operation | Target | Max |
|-----------|--------|-----|
| UI render | <16ms | 33ms |
| Drag feedback | <50ms | 100ms |
| Deploy click → feedback | <100ms | 200ms |
| Validation | <500ms | 1s |
| Build start → first log | <1s | 2s |

## Success Metrics

### Quantitative
- Time to first deploy: <5 minutes
- Success rate: >95%
- Error recovery rate: >90%
- User retention: >80% (return after 1 week)

### Qualitative
- "This is easy" (not "this is powerful")
- "I get it" (not "I think I understand")
- "Finally!" (not "okay, fine")
- Shows friends without prompting

## Anti-Patterns to Avoid

### ❌ Wizard Hell
Don't: Multi-step wizards with 10 screens
Do: Single screen with smart defaults

### ❌ Option Paralysis
Don't: 50 configuration options
Do: 3 options + "Advanced" button

### ❌ Cryptic Abbreviations
Don't: "Configure RBAC for K8s ingress"
Do: "Set up access control"

### ❌ Hidden Power
Don't: Hide important features in menus
Do: Show what's possible immediately

### ❌ Death by Dialog
Don't: "Are you sure? This action cannot be undone!"
Do: Just do it + undo button

## The Delight Factor

**Every user should have at least 3 "wow" moments**:

### Moment 1: First Launch
- Beautiful UI loads instantly
- Clear visual hierarchy
- "I know what to do" without instructions

### Moment 2: First Deploy
- Drag, drop, click Deploy
- "Wait, that's it?"
- Works on first try

### Moment 3: First Error
- Something goes wrong
- Error message is friendly
- Click [Fix] → Problem solved
- "That was... easy?"

## Testing Checklist

### Before Each Release
- [ ] Can deploy LAMP stack in <5 minutes
- [ ] All error messages have [Fix] button
- [ ] No operation takes >2s without feedback
- [ ] Keyboard navigation works everywhere
- [ ] Screen reader announces all actions
- [ ] Dark mode looks good
- [ ] Passes "Grandmother Test"
- [ ] Container-hater can use it successfully

## The Ultimate Question

**"Would your son use this?"**

If not, keep iterating.

If yes, ship it. 🚀
