# 🌾 Smart Co-op Management System

A decentralized agricultural cooperative management system built on the Stacks blockchain using Clarity smart contracts. This system enables farming cooperatives to pool resources, vote on collective decisions, and distribute profits fairly among members.

## 🚀 Features

- 👥 **Member Management**: Join cooperatives with initial contributions
- 💰 **Resource Pooling**: Add funds to the collective pool
- 🗳️ **Democratic Voting**: Create and vote on proposals for purchases and investments  
- 📊 **Proposal System**: Track voting progress and execute approved proposals
- 💸 **Profit Distribution**: Fair distribution of cooperative profits to all members
- 🔍 **Transparency**: All transactions and votes are recorded on-chain

## 📋 Contract Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `join-coop` | Join the cooperative with an initial STX contribution |
| `add-funds` | Add additional funds to your contribution |
| `create-proposal` | Create a new proposal for voting |
| `vote-on-proposal` | Vote for or against a proposal |
| `execute-proposal` | Execute an approved proposal after voting period |
| `distribute-profits` | Distribute profits to all members (owner only) |
| `claim-profit` | Claim your share of distributed profits |
| `leave-coop` | Leave the cooperative |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-member-info` | Get member details and contribution |
| `get-proposal` | Get proposal details by ID |
| `get-pool-funds` | Get total pooled funds |
| `get-member-count` | Get active member count |
| `get-vote` | Check how a member voted on a proposal |
| `get-profit-distribution` | Get profit distribution details |
| `has-claimed-profit` | Check if member claimed their profit share |

## 🛠️ Usage Examples

### Joining the Cooperative
```clarity
(contract-call? .Smart-Co-op-Management-System join-coop u1000000)
