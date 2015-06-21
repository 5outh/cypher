# cypher
Haskell client for Neo4j

Goals of this library:

- Generic Node and Relationship types for direct manipulation of Neo4j nodes and relationships
- Node/Relationship typeclasses for inserting typed Neo4j entries
- Simple finding logic
  - Parses into types
  - ...or directly manipulable
- Wrappers for all REST API endpoints, relatively untouched.

Wishlist:

- Template Haskell for defining Cypher Queries
