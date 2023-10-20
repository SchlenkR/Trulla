# Trulla Templates for Node CLI

[![npm version](https://badge.fury.io/js/trulla-templates.svg)](https://badge.fury.io/js/trulla-templates)


Example-Project
---

Have a look at the [HowTo example]([./src/NodeCli/Example/](https://github.com/RonaldSchlenker/Trulla/tree/main/src/NodeCli/Example))


Step by Step Guide
---

Install the package globally:

```bash
npm install -g trulla-templates
```

Create a trulla template:

```bash
mkdir my-trulla-templates
cd my-trulla-templates
```

Create new template file called `TestTemplate.trulla` with the following content:

```
Hello {{user.name}}, how are you?

Your Orders
===
{{for order in orders|---}}
ID: {{order.id}}
({{if order.isActive}}active{{else}}inactive{{end}})
{{end}}
```

To render the template, execute the Trulle CLI:

```bash
trulla -i . -o ./out
```

This will create a new file `TestTemplate.ts` in the `out` directory.

Next, create a new file `test.ts` with the following content:

```typescript
import { render } from './out/TestTemplate';

const renderedTemplate = render({
  orders: [
    {
      id: '0',
      isActive: true,
    },
    {
      id: '1',
      isActive: false,
    },
    {
      id: '2',
      isActive: true,
    },
  ],
  user: {
    name: 'Hans',
  },
});

console.log(renderedTemplate);
```

Run it with:

```bash
npx ts-node test.ts
```

This will print the rendered template to console:

```
Hello Hans, how are you?

Your Orders
===

ID: 0
(active)
---
ID: 1
(inactive)
---
ID: 2
(active)
```
