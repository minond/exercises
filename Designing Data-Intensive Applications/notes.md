# Chapter 1

High percentiles of response times are important because they directly affect
users's expericen of the service. For example, Amazon describes response time
requirements for internal services in terms of p999, even though it only
affects 1 in 1,000 requests. This is because the customers with the slowest
requests are often those who have the most data on their accounts because they
have made many purchases -- that is, they're the most valuable customers. ...
Amazon has observed that a 100 ms increase in response time reduces sales by
1%, and others report that a 1-second slowdown reduces a customer satisfaction
metric by 16%.

On the other hand, optimizing the p9999 was deemed too expensive and to not
yield enough benefit for Amazon's purposes.


# Chapter 2

```sql
select date_truct('month', observation_timestamp) as observation_month,
  sum(num_animals) as total_animals
from observations
where family = 'Sharks'
group by observation_month;
```

```js
db.observations.mapReduce(
  function map() {
    var year = this.observationTimestamp.getFullYear()
    var month = this.observationTimestamp.getMonth() + 1
    return [year + "-" + month, this.numAnimals]
   },
   function reduce(key, values) {
    return Array.sum(values)
  },
  {
    query: { family: "Sharks" },
    out: "monthlySharkReport",
  }
)
```

The `map` function is called once for every document that matches the query,
and it emits a key and a value. The key-value pairs emitted by `map` are
grouped by key and for all key-value pairs with the same key, the `reduce`
function is called once.

Remember that graphs are made up of vertices (nodes, entities) and edges
(relationships, arcs). Examples:

- Social graphs: vertices are people, and edges indicate which people know each
  other.
- Web graph: vertices are web pages, and edges indicate HTML links to other
  pages.
- Road or rail networks: vertices are junctions, and edges represent the roads
  or railway lines between them.

Properties of graphs:

- Vertex is made up of
  1. a unique identifier
  2. a set of outgoing edges
  3. a set of incoming edges
  4. a collection of properties (key-value pairs)

- An Edge is made up of
  1. a unique identifier
  2. the vertex at which the edge starts (tail vertex)
  3. the vertex at which the edge ends (head vertex)
  4. a label to describe the kind of relationship
  5. a collection of properties (key-value pairs)


Represented in a relational schema, a graph database may look like this:

```sql
create table vertices (
  id integer primary key,
  properties json
);

create table edges (
  id integer primary key,
  head_vertex_id integer references vertices (id),
  tail_vertex_id integer references vertices (id),
  label text,
  properties json
);

create index edges_heads on edges (head_vertex_id);
create index edges_tails on edges (tail_vertex_id);
```

```cypher
CREATE
  (NAmerica:Location {name: 'North America', type: 'continent'}),
  (USA:Location      {name: 'United States', type: 'country'}),
  (Idaho:Location    {name: 'Idaho',         type: 'state'})

CREATE
  (Lucy:Person       {name: 'Lucy'})

CREATE
  (Idaho) -[:WITHIN]-> (USA) -[:WITHIN]-> (NAmerica),
  (Lucy) -[:BORN_IN]-> (Idaho)
```

```cypher
MATCH
  (person) -[:BORN_IN]->  () -[:WITHIN*0..]-> (us:Location {name: 'United States'}),
  (person) -[:LIVES_IN]-> () -[:WITHIN*0..]-> (eu:Location {name: 'Europe'})
RETURN person.name
```

_Query to find people who emigrated from the US to Europe_ which can be read as
follows: find any vertex (call it person) that meets both of the following
conditions: (1) persona has an outgoing BORN_IN edge to some vertex. From that
vertex, you can follow a chain of outgoing WITHIN edges until eventually you
reach a vertex of type Location, whose name property is equal to "United
States". (2) That same person vertex also has an outgoing LIVES_IN edge.
Following that edge, and then a chain of outgoing WITHIN edges, you eventually
reach a vertex of type Location, whose name propery is equal to "Europe".
