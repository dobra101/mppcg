package dobra101.mppcg.adapter.sablecc

import de.be4.classicalb.core.parser.node.ADeferredSetSet
import de.be4.classicalb.core.parser.node.ADescriptionSet
import de.be4.classicalb.core.parser.node.AEnumeratedSetSet
import de.be4.classicalb.core.parser.node.AEnumeratedSetViaDefSet
import dobra101.mppcg.node.collection.CollectionNode
import dobra101.mppcg.node.collection.EnumCollectionNode
import dobra101.mppcg.node.collection.EnumEntry
import dobra101.mppcg.node.expression.IdentifierExpression

class CollectionVisitor : AbstractVisitor() {

    override var result: CollectionNode? = null

    override fun caseAEnumeratedSetSet(node: AEnumeratedSetSet) {
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

    override fun caseADescriptionSet(node: ADescriptionSet) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseADeferredSetSet(node: ADeferredSetSet) {
        TODO("Not implemented ${node::class.simpleName}")
    }

    override fun caseAEnumeratedSetViaDefSet(node: AEnumeratedSetViaDefSet) {
        TODO("Not implemented ${node::class.simpleName}")
    }
}