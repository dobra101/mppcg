package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.AEnumeratedSetSet
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.collection.EnumCollectionNode
import dobra101.mppcg.node.collection.EnumEntry
import dobra101.mppcg.node.expression.IdentifierExpression

class CollectionVisitor : AbstractVisitor() {

    override var result: CollectionNode? = null

    // TODO: not every set to enum
    override fun caseAEnumeratedSetSet(node: AEnumeratedSetSet) {
        // TODO: when more than one identifier?
        val collectionName = node.identifier.first.text
        result = EnumCollectionNode(
            name = collectionName,
            elements = node.elements.convert().map {
                EnumEntry(
                    name = (it as IdentifierExpression).name,
                    enum = collectionName
                )
            }
        )
    }
}