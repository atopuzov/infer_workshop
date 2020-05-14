package types

import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf
import syntax.*
import kotlin.Exception

inline class Substitution(val subst: HashMap<Int, Monotype> = hashMapOf()) {
    fun apply(ty: Monotype): Monotype {
        return when (ty) {
            Monotype.Int,
            Monotype.String,
            Monotype.Bool -> ty
            is Monotype.Unknown -> subst[ty.u]?.let { apply(it) } ?: ty
            is Monotype.Function -> Monotype.Function(apply(ty.argument), apply(ty.result))
        }
    }

    operator fun set(u: Int, ty: Monotype) {
        subst[u] = ty
    }

    override fun toString(): String =
        "{ " + subst.toList().joinToString("\n, ") { (u, ty) -> "u$u ↦ ${ty.pretty()}" } + "\n}"
}

inline class Environment(val env: PersistentMap<Name, Monotype> = persistentHashMapOf()) {
    operator fun get(name: Name): Monotype? = env[name]
    fun extend(name: Name, ty: Monotype) = Environment(env.put(name, ty))
}

class TypeChecker {
    var freshSupply: Int = 0
    var substitution: Substitution = Substitution()

    // Returns a fresh `Unknown`, where fresh means "not ever used before"
    private fun freshUnknown(): Monotype = Monotype.Unknown(++freshSupply)

    // Applies the current substitution to a given type
    fun zonk(ty: Monotype): Monotype = substitution.apply(ty)

    fun unify(ty1: Monotype, ty2: Monotype) {
        val ty1 = zonk(ty1)
        val ty2 = zonk(ty2)
        throw Exception("Can't match ${ty1.pretty()} with ${ty2.pretty()}")
    }

    private fun infer(env: Environment, expr: Expression): Monotype {
        return when (expr) {
            is Expression.Int -> Monotype.Int
            is Expression.String -> Monotype.String
            is Expression.Bool -> Monotype.Bool
            is Expression.Var -> env[expr.name] ?: throw Exception("Unknown variable ${expr.name}")
            is Expression.Let -> {
                val tyExpr = infer(env, expr.expr)
                val newEnv = env.extend(expr.binder, tyExpr)
                val tyBody = infer(newEnv, expr.body)
                tyBody
            }
            is Expression.Lambda -> {
                val tyArg = freshUnknown()
                val newEnv = env.extend(expr.binder, tyArg)
                val tyBody = infer(newEnv, expr.body)
                Monotype.Function(tyArg, tyBody)
            }
            is Expression.App -> TODO()
            is Expression.If -> TODO()
        }
    }

    fun inferExpr(env: Environment, expr: Expression): Monotype = zonk(infer(env, expr))
}