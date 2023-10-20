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
