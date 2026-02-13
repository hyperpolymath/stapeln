# Cerro Torre Cooperative: Articles of Governance

**Version**: Draft 0.1  
**Status**: Pre-formation working document

## 1. Name and Purpose

### 1.1 Name

The organisation shall be known as the **Cerro Torre Cooperative** (the "Cooperative").

### 1.2 Purpose

The Cooperative exists to:

1. Develop and maintain the Cerro Torre Linux distribution and associated tooling
2. Provide supply-chain-verified container images to the public
3. Advance the state of formally verified systems software
4. Demonstrate that democratic governance of critical infrastructure is viable
5. Operate transparently and accountably to its members and the public

### 1.3 Non-Profit Character

The Cooperative is not operated for private profit. Any surplus shall be reinvested in the Cooperative's mission or distributed to members in proportion to their participation, never to outside investors.

## 2. Membership

### 2.1 Membership Classes

The Cooperative has three classes of members:

**Maintainer Members**: Individuals who actively contribute to package maintenance, infrastructure, documentation, or governance. Maintainer members have voting rights on all matters.

**User Members**: Organisations or individuals who use Cerro Torre in production and choose to support the Cooperative financially. User members have voting rights on strategic matters but not on technical decisions about specific packages.

**Worker Members**: Individuals employed by the Cooperative (if any). Worker members have voting rights equivalent to Maintainer Members, plus specific rights regarding employment conditions.

### 2.2 Admission

**Maintainer Members** are admitted by:
1. Making sustained contributions over at least 3 months, AND
2. Affirming the Palimpsest Covenant, AND  
3. Receiving approval from a majority of existing Maintainer Members

**User Members** are admitted by:
1. Paying the applicable membership fee (sliding scale based on organisation size), AND
2. Affirming the Palimpsest Covenant

**Worker Members** are admitted automatically upon employment by the Cooperative.

### 2.3 Resignation and Removal

Members may resign at any time by written notice.

Members may be removed for:
1. Sustained violation of the Palimpsest Covenant
2. Actions that materially harm the Cooperative or its mission
3. Inactivity exceeding 24 months (Maintainer Members only)

Removal requires:
- A formal complaint submitted to the Governance Committee
- An opportunity for the member to respond
- A 2/3 supermajority vote of the relevant membership class

### 2.4 No Transferability

Membership is personal and may not be sold, assigned, or transferred.

## 3. Governance Structure

### 3.1 General Assembly

The **General Assembly** is the highest decision-making body, comprising all members. It meets at least annually (remotely or in person) and may be convened by:
- The Coordinating Council
- A petition signed by 10% of members

The General Assembly:
- Elects the Coordinating Council
- Amends these Articles
- Approves strategic plans
- Decides dissolution or merger
- Resolves disputes not resolved at lower levels

### 3.2 Coordinating Council

The **Coordinating Council** manages the Cooperative's affairs between General Assemblies. It comprises 5-9 members elected by the General Assembly for 2-year staggered terms.

The Council must include:
- At least 3 Maintainer Members
- At least 1 User Member (if any exist)
- At least 1 Worker Member (if any exist)

The Council:
- Sets operational policy
- Manages finances
- Represents the Cooperative externally
- Appoints committees
- Hires staff (subject to budget approval)

Council decisions require a simple majority. No member may serve more than three consecutive terms.

### 3.3 Technical Committee

The **Technical Committee** has authority over technical matters, including:
- Package acceptance and removal
- Tooling architecture decisions
- Security policy
- Release management

Technical Committee members are Maintainer Members elected by Maintainer Members for 2-year terms. The committee operates by consensus where possible; disputed matters require 2/3 majority.

### 3.4 Governance Committee

The **Governance Committee** handles:
- Membership disputes
- Code of conduct enforcement
- Covenant interpretation
- Election administration

Governance Committee members are elected by the General Assembly and may not simultaneously serve on the Coordinating Council.

## 4. Decision-Making

### 4.1 Principles

Decisions should be made at the lowest appropriate level:
- Individual maintainers decide about their packages
- The Technical Committee decides cross-cutting technical matters
- The Coordinating Council decides operational matters
- The General Assembly decides strategic and constitutional matters

### 4.2 Voting

**Ordinary decisions**: Simple majority of those voting, with a quorum of 25% of eligible voters.

**Significant decisions** (budget over £10,000, policy changes, new membership classes): 2/3 majority with 40% quorum.

**Constitutional decisions** (amending Articles, dissolution, merger): 3/4 majority with 50% quorum.

All votes shall be conducted transparently. Secret ballots may be used for elections and personnel matters.

### 4.3 Consensus Preference

Before calling a vote, decision-makers should attempt to reach consensus through discussion. Votes are a fallback for when consensus cannot be achieved, not the default.

### 4.4 Documentation

All decisions shall be documented in a public decision log, including:
- What was decided
- Who participated
- The reasoning
- Any dissenting views

## 5. Assets and Finance

### 5.1 Asset Lock

The Cooperative's assets are held in trust for its mission. Upon dissolution:
1. Debts and liabilities shall be paid
2. Remaining assets shall be transferred to another cooperative, charity, or community organisation with compatible purposes
3. Assets may **never** be distributed to members as individuals or to any for-profit entity

### 5.2 Prohibition on Sale

The Cooperative may not be sold, acquired, or merged with a for-profit entity. Any merger must be with another cooperative or non-profit organisation and requires 3/4 approval of the General Assembly.

### 5.3 Transparency

Financial records shall be available to all members. An annual financial report shall be published publicly.

## 6. Intellectual Property

### 6.1 Licensing

All software produced by the Cooperative shall be released under open source licenses approved by the Open Source Initiative. The default licenses are PMPL-1.0-or-later (user's choice).

### 6.2 Trademarks

The Cooperative may hold trademarks to protect the integrity of the project. Trademark policy shall balance protection against misuse with permissive use for legitimate purposes.

### 6.3 No Assignment Required

Contributors retain copyright in their contributions. The Cooperative does not require copyright assignment. Contributors grant a license sufficient for the Cooperative to distribute their contributions under the project's licenses.

### 6.4 Contributor Licensing

All contributions must be made under the Developer Certificate of Origin (DCO), signed off with each commit. No Contributor License Agreement (CLA) is required.

## 7. Forking

### 7.1 Right to Fork

The right to fork is fundamental to free software and is explicitly protected. Any person may fork Cerro Torre at any time for any reason.

### 7.2 Trademark Limitations

Forks may not use the "Cerro Torre" name or logo without permission. This is the sole limitation on forking.

### 7.3 Cooperative Response

If a fork emerges due to governance failures, the Cooperative shall treat this as a signal to examine and address underlying problems, not as an attack to be resisted.

## 8. Amendments

These Articles may be amended by:
1. A proposal submitted to the Coordinating Council
2. A 30-day comment period
3. A 3/4 majority vote of the General Assembly with 50% quorum

No amendment may:
- Remove the asset lock (Section 5.1)
- Remove the prohibition on sale (Section 5.2)
- Remove the right to fork (Section 7.1)
- Remove the requirement for open source licensing (Section 6.1)

## 9. Dissolution

The Cooperative may be dissolved by a 3/4 vote of the General Assembly with 60% quorum, after:
1. A 90-day notice period
2. Good-faith effort to find an organisation to continue the mission

Upon dissolution, the asset lock (Section 5.1) applies.

## 10. Initial Formation

### 10.1 Bootstrapping

Until the Cooperative is formally incorporated and has 5 Maintainer Members:
- The founder(s) act as interim Coordinating Council
- New Maintainer Members may be admitted by founder approval
- These Articles apply as written policy, not yet as legal bylaws

### 10.2 Incorporation

The Cooperative shall incorporate as a legal entity (Community Interest Company, Cooperative Society, or equivalent) within 12 months of reaching 5 Maintainer Members, or sooner if funds permit.

### 10.3 Transition

Upon incorporation:
- These Articles become the legal governing documents (adapted as required by law)
- An initial General Assembly elects proper governance bodies
- Interim decisions are ratified or revised

---

## Appendix: Founding Values

These Articles encode the following values:

**Democracy**: Those who do the work govern the project. No benevolent dictator, no corporate owner.

**Transparency**: Decisions are made in public, documented, and explained.

**Sustainability**: The asset lock and sale prohibition ensure the Cooperative cannot be captured or extracted from.

**Humility**: The right to fork is protected because we might be wrong. Competition keeps us honest.

**Solidarity**: The Cooperative exists to serve its community, not to accumulate power or wealth.

---

*Drafted December 2024. To be ratified upon formation.*
